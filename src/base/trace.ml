(**************************************************************************)
(*  This file is part of COLORSTREAMS.                                    *)
(*                                                                        *)
(*  Copyright (C) 2025                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

let more_opts = ref ""
let target_args = ref ""
let main = ref ""
let timeout = ref (-1)

module Wrap =
    struct
        type Message.Wrap.wrap +=
            | Trace of string Lazy.t
            | Output of string
            | Crash
            | Timeout

        type Message.ext_value += TargetCrashed | GeneralTimeout

        let unwrapper next = function
            | Trace(s) -> 5, false, "TRACE", Message.Value(s), s
            | Output(s) -> 2, false, "OUTPUT", Message.Value(lazy s), lazy s
            | Crash -> 1, true, "ERROR", TargetCrashed, lazy "target crashed"
            | Timeout -> 1, true, "ERROR", GeneralTimeout, lazy "general timeout"
            | w -> next w

        let _ = Message.Wrap.register_unwrapper unwrapper
    end

module Gdb =
    struct
        type session = 
            {
                proc : Subprocess.session;
                qcnt : Stats.counter;
                timer : Stats.timer;
            }

        let create_session () =
            let proc = Subprocess.create_session "gdb" @@ Printf.sprintf "gdb --interpreter=mi3 -iex \"set print frame-arguments none\" -iex \"set source open off\" %s" @@ Options.get_target () in
            let qcnt = new Stats.counter "qcnt" "Gdb queries" 0 in
            let timer = new Stats.timer "gdb timer" "Gdb runtime" in
            {proc; qcnt; timer}

        let close_session s =
            Subprocess.send s.proc "q";
            {s with proc = Subprocess.close_session s.proc}

        let kill s =
            {s with proc = Subprocess.close_session @@ Subprocess.kill s.proc}

        let continue s =
            Subprocess.send s.proc "c"

        let target_remote s port =
            Subprocess.send s.proc @@ Printf.sprintf "target remote :%d" port

        module ResVal =
            struct
                let resrec_exp = Str.regexp {|\([0-9]+\)\^\(done\|running\|connected\|error\|exit\)\(.*$\)|}
                let res_exp = Str.regexp {|\([^=[{]*\)=\(.*$\)|}
                let list_exp = Str.regexp {|\[\(.*\)\]$|}
                let tuple_exp = Str.regexp {|{\(.*\)}$|}
                let const_exp = Str.regexp {|"\(.*\)"$|}

                type resclass = Done | Running | Connected | Error | Exit
                type value = Const of string | Tuple of (string * value) list | List of value list | ResList of (string * value) list
                type t = 
                    {
                        token : int option;
                        cmd : string;
                        resclass : resclass;
                        res : (string * value) list
                    }

                let parse cmd out =
                    let split_list s =
                        let max = String.length s in
                        let rec aux in_string escaped last level i res =
                            if i = max
                            then (String.sub s (last + 1) (i - last - 1))::res
                            else
                            (
                                match in_string, s.[i] with
                                | true, '\\' -> aux true true last level (i + 1) res
                                | false, '"' -> aux true false last level (i + 1) res
                                | true, '"' -> aux escaped false last level (i + 1) res
                                | false, '{'
                                | false, '[' -> aux false false last (level + 1) (i + 1) res
                                | false, '}'
                                | false, ']' -> aux false false last (level - 1) (i + 1) res
                                | false, ',' ->
                                (
                                    if level = 0
                                    then aux false false i level (i + 1) ((String.sub s (last + 1) (i - last - 1))::res)
                                    else aux false false last level (i + 1) res
                                )
                                | _ -> aux in_string false last level (i + 1) res
                            )
                        in
                        List.rev @@ aux false false (-1) 0 0 []
                    in
                    let rec parse_result s =
                        let rec parse_value s =
                            let parse_reslist l =
                                let aux e res =
                                    match res, parse_result e with
                                    | Some(res), Some(r) -> Some(r::res)
                                    | _, _ -> None
                                in
                                List.fold_right aux l (Some([]))
                            in
                            if Str.string_match tuple_exp s 0
                            then 
                            (
                                let l = split_list @@ Str.matched_group 1 s in
                                match l, parse_reslist l with
                                | _, Some(l) -> Tuple(l)
                                | [""], _ -> Tuple([])
                                | _ -> raise (Failure (Printf.sprintf "could not parse tuple from <%s>" s))
                            )
                            else if Str.string_match list_exp s 0
                            then 
                            (
                                let l = split_list @@ Str.matched_group 1 s in
                                match l, parse_reslist l with
                                | _, Some(l) -> ResList(l)
                                | [""], _ -> List([])
                                | _ -> List(List.map parse_value l)
                            )
                            else if Str.string_match const_exp s 0
                            then Const(Str.matched_group 1 s)
                            else Const(s)
                        in
                        if Str.string_match res_exp s 0
                        then
                        (
                            let variable = Str.matched_group 1 s in
                            let value = parse_value @@ Str.matched_group 2 s in
                            Some((variable, value))
                        )
                        else None
                    in
                    let rec parse_line = function
                        | line::t ->
                        (
                            if Str.string_match resrec_exp line 0
                            then
                            (
                                try
                                    let token = 
                                        try
                                            Some(int_of_string @@ Str.matched_group 1 line) 
                                        with Not_found -> None
                                    in
                                    let resclass = 
                                        match Str.matched_group 2 line with
                                        | "done" -> Done
                                        | "running" -> Running
                                        | "connected" -> Connected
                                        | "exit" -> Exit
                                        | _ -> Error
                                    in
                                    let clean l =
                                        List.fold_right
                                        (
                                            fun e res ->
                                            (
                                                match e with
                                                | Some(e) -> e::res
                                                | _ -> res
                                            )
                                        )
                                        l []
                                    in
                                    let res = Str.matched_group 3 line 
                                        |> split_list
                                        |> List.map parse_result
                                        |> clean
                                    in
                                    {token; cmd; resclass; res}
                                with err -> 
                                (
                                    let value = Const(Printexc.to_string err) in 
                                    {token = None; cmd; resclass = Error; res = ["Exception", value]}
                                )
                            )
                            else parse_line t
                        )
                        | [] -> {token = None; cmd; resclass = Error; res = []}
                    in
                    parse_line out

                let to_generic r =
                    let rec to_generic_res (label, value) =
                        let rec to_generic_val v =
                            match v with
                            | Const(s) -> Result.Str(s)
                            | Tuple(t) -> Result.Res(List.map to_generic_res t)
                            | List(l) -> 
                            (
                                let aux i e =
                                    let variable = Printf.sprintf "%d" i in
                                    to_generic_res (variable, e)
                                in
                                Result.Res(List.mapi aux l)
                            )
                            | ResList(rl) -> 
                            (
                                let aux i e =
                                    let label = Printf.sprintf "%d" i in
                                    Result.mk_res_list ~label [e]
                                in
                                Result.Res(List.mapi aux @@ List.map to_generic_res rl)
                            )
                        in
                        match to_generic_val value with
                        | Result.Str(s) -> Result.mk_string ~label s
                        | Result.Res(rl) -> Result.mk_res_list ~label rl
                        | Result.Lst(l) -> Result.mk_list ~label l
                        | _ -> assert false
                    in
                    let tokres = 
                        match r.token with
                        | Some(token) -> Result.mk_int ~label:"token" @@ Z.of_int token
                        | None -> Result.mk_bool ~label:"token" false
                    in
                    let cmdres = Result.mk_string ~label:"query" r.cmd in
                    let classres = 
                        let s =
                            match r.resclass with
                            | Done -> "done"
                            | Connected -> "connected"
                            | Running -> "running"
                            | Error -> "error"
                            | Exit -> "exit"
                        in
                        Result.mk_string ~label:"class" s
                    in
                    let res = Result.mk_res_list ~label:"results" @@ List.map to_generic_res r.res in
                    Result.Res([tokres; cmdres; classres; res])
            end

        module Res = Result.Make (ResVal)

        let query s cmd =
            let start = s.timer#get in
            s.timer#start;
            Subprocess.send s.proc @@ Printf.sprintf "%d%s" (s.qcnt#get) cmd;
            let prefix = Printf.sprintf "%d^" s.qcnt#get in
            let label = Printf.sprintf "gdb query %d" s.qcnt#get in
            s.qcnt#incr;
            let line = Subprocess.receive_until s.proc (fun line -> String.starts_with ~prefix line) in
            Message.Wrap.send (Message.Wrap.Debug("GDB", lazy (Printf.sprintf "gdb query runtime: %.3g" (s.timer#get -. start))));
            s.timer#stop;
            Res.create ~label @@ ResVal.parse cmd line

        let bad_value_exp = Str.regexp {|<incomplete type>\|<optimized out>|}

        let get_locals session = 
            ignore @@ query session "-foobar";
            let res = query session "-stack-list-variables 0" in
            try
                match res.result.resclass with
                | Done ->
                (
                    let get_var_name entry res =
                        match entry with
                        | ResVal.Tuple(("name", ResVal.Const(name))::_) -> name::res
                        | _ -> res
                    in
                    match res.result.res with
                    | ["variables", List(locals)] -> List.fold_right get_var_name locals []
                    | _ -> raise (Failure "malformed locals list")
                )
                | _ -> raise (Failure "no result")
            with err ->
            (
                Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "failed to get local variable info from gdb:\n%s\n%s" (Result.pp @@ Res.export res) @@ Printexc.to_string err)));
                []
            )

        let get_globals session =
            let res = query session "-symbol-info-variables" in
            try
                match res.result.resclass with
                | Done ->
                (
                    match res.result.res with
                    | ["symbols", ResVal.Tuple(vars)] ->
                    (
                        try
                            match List.assoc "debug" vars with
                            | List(l) ->
                            (
                                let get_globs e res =
                                    match e with
                                    | ResVal.Tuple(l) ->
                                    (
                                        let new_globs =
                                            try
                                                let file =
                                                    match List.assoc "fullname" l with
                                                    | Const(path) -> File.from_file path
                                                    | _ -> raise Not_found
                                                in
                                                match List.assoc "symbols" l with
                                                | List(l) ->
                                                (
                                                    let get_var e res =
                                                        match e with
                                                        | ResVal.Tuple(l) -> 
                                                        (
                                                            try
                                                                match List.assoc "name" l with
                                                                | Const(name) -> (file, name)::res
                                                                | _ -> res
                                                            with Not_found -> res
                                                        )
                                                        | _ -> res
                                                    in
                                                    List.fold_right get_var l []
                                                )
                                                | _ -> []
                                            with Not_found -> []
                                        in
                                        new_globs @ res
                                    )
                                    | _ -> res
                                in
                                List.fold_right get_globs l []
                            )
                            | _ -> raise (Failure "malformed symbols list")
                        with Not_found -> []
                    )
                    | _ -> raise (Failure "malformed result")
                )
                | _ -> raise (Failure "no result")
            with err ->
            (
                Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "failed to get global variables info from gdb:\n%s\n%s" (Result.pp @@ Res.export res) @@ Printexc.to_string err)));
                []
            )

        let numexp = Str.regexp {|^[^< ]*\(0x\)?[0-9a-fA-F]+|}

        let eval_expr ?(silent = false) session expr =
            let res = query session @@ Printf.sprintf "-data-evaluate-expression %s" expr in
            try
                match res.result.resclass, res.result.res with
                | Done, ["value", Const(value)] -> 
                (
                    ignore @@ Str.search_forward numexp value 0;
                    Some(Z.of_string @@ Str.matched_string value)
                )
                | _ -> raise (Failure "error")
            with err ->
            (
                if not silent
                then Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "failed to evaluate expression <%s>:\n%s\n%s" expr (Result.pp @@ Res.export res) @@ Printexc.to_string err)))
                ;
                None
            )

        module Location =
            struct
                type t = 
                    {
                        addr : Address.t option;
                        func : string option;
                        file : string option;
                        line : string option;
                        arch : string option;
                        caller : t option
                    }

                let create addr func file line arch caller =
                    {addr; func; file; line; arch; caller}

                let to_generic loc =
                    let rec aux loc =
                        let aux_ resf s l =
                            match s with
                            | Some(s) -> (resf s)::l
                            | None -> l
                        in
                        let aux__ resf s l =
                            match s with
                            | Some(s) -> (resf @@ aux s)::l
                            | None -> l
                        in
                        []
                            |> aux__ (Result.mk_res_list ~label:"caller") loc.caller
                            |> aux_ (Result.mk_string ~label:"arch") loc.arch
                            |> aux_ (Result.mk_string ~label:"line") loc.line
                            |> aux_ (Result.mk_string ~label:"file") loc.file
                            |> aux_ (Result.mk_string ~label:"func") loc.func
                            |> aux_ (Result.mk_int ~label:"addr") loc.addr
                    in
                    Result.Res(aux loc)

                let rec of_generic loc = 
                    List.fold_left
                        (
                            fun res e ->
                                match e.Result.label, e.Result.result with
                                | "addr", Result.Int(addr) -> {res with addr = Some(addr)}
                                | "func", Str(func) -> {res with func = Some(func)}
                                | "file", Str(file) -> {res with file = Some(file)}
                                | "line", Str(line) -> {res with line = Some(line)}
                                | "arch", Str(arch) -> {res with arch = Some(arch)}
                                | "caller", Res(caller) -> {res with caller = Some(of_generic caller)}
                                | _ -> assert false
                        )
                        {addr = None; func = None; file = None; line = None; arch = None; caller = None} loc

                let custom_printer loc =
                    let pp_line loc =
                        (
                            match loc.file with
                            | Some(file) -> file ^ " : "
                            | _ -> "??? : "
                        ) ^
                        (
                            match loc.func with
                            | Some(func) -> func ^ " : "
                            | _ -> "??? : "
                        ) ^
                        (
                            match loc.line with
                            | Some(line) -> line
                            | _ -> "???"
                        )
                    in
                    let rec aux res = function
                        | Some(loc) -> aux ((pp_line loc)::res) loc.caller
                        | _ -> res
                    in
                    List.rev @@ aux [] (Some(loc))
            end

        module LocRes = Result.Make (Location)

        let get_location_from_generic_result res =
            let loc =
                match res.Result.result with
                | Result.Res(loc) -> loc
                | _ -> assert false
            in
            LocRes.create ~label:res.label @@ Location.of_generic loc

        let get_location session =
            let res = query session "-stack-list-frames" in
            try
                match res.result.resclass, res.result.res with
                | Done, [("stack", ResVal.ResList(frames))] ->
                (
                    let aux (label, frame) res =
                        if label = "frame"
                        then
                        (
                            let get_maybe_const = function
                                | Some(ResVal.Const(s)) -> Some(s)
                                | _ -> None
                            in
                            match frame with
                            | ResVal.Tuple(frame) ->
                            (
                                let addr = 
                                    match List.assoc_opt "addr" frame with
                                    | Some(ResVal.Const(addr)) -> Some(Address.of_string addr)
                                    | _ -> None
                                in
                                let func = get_maybe_const @@ List.assoc_opt "func" frame in
                                let file = 
                                    match (List.assoc_opt "fullname" frame), List.assoc_opt "from" frame with
                                    | Some(Const(file)), _
                                    | _, Some(Const(file)) -> Some(file)
                                    | _ -> None
                                in
                                let line = get_maybe_const @@ List.assoc_opt "line" frame in
                                let arch = get_maybe_const @@ List.assoc_opt "arch" frame in
                                Some(Location.create addr func file line arch res)
                            )
                            | _ -> res
                        )
                        else res
                    in
                    match List.fold_right aux frames None with
                    | Some(loc) -> 
                    (
                        let locres = LocRes.create ~label:"location" loc in
                        Some(LocRes.set_custom_printer (Some(lazy (Location.custom_printer loc))) locres)
                    )
                    | _ -> None
                )
                | _ -> None
            with _ -> None
    end

module TraceStats =
    struct
        type t =
            {
                name : Stats.sstat;
                queries : Stats.counter;
                runtime : Stats.timer;
                tracerruntime : Stats.timer;
                parsing : Stats.timer;
                instrcache : Stats.counter;
                synccnt : Stats.counter;
                asynccnt : Stats.counter;
            }

        let create tracer =
            let name = new Stats.sstat "tracer" "Tracer" in
            name#set tracer;
            let queries = new Stats.counter "query cnt" "Queries" 0 in
            let runtime = new Stats.timer "runtime" "Tracer runtime (target execution)" in
            let tracerruntime = new Stats.timer "tracer runtime" "Tracer runtime (all)" in
            let parsing = new Stats.timer "parsing" "Trace parsing runtime" in
            let instrcache = new Stats.counter "instr cache" "Instruction cache updates" 0 in
            let synccnt = new Stats.counter "sync cnt" "Synchronization queries" 0 in
            let asynccnt = new Stats.counter "async cnt" "Asynchronous updates" 0 in
            {name; queries; runtime; tracerruntime; parsing; instrcache; synccnt; asynccnt}

        let to_bundle name s =
            Stats.Bundle.create name
                |> Stats.Bundle.add_stat (s.name :> Stats.stat)
                |> Stats.Bundle.add_stat (s.queries :> Stats.stat)
                |> Stats.Bundle.add_stat (s.instrcache :> Stats.stat)
                |> Stats.Bundle.add_stat (s.synccnt :> Stats.stat)
                |> Stats.Bundle.add_stat (s.asynccnt :> Stats.stat)
                |> Stats.Bundle.add_stat (s.runtime :> Stats.stat)
                |> Stats.Bundle.add_stat (s.tracerruntime :> Stats.stat)
                |> Stats.Bundle.add_stat (s.parsing :> Stats.stat)
    end

type trace = Ins of Instruction.t | Fun of Function.t | Src of Source.t | Snk of Sink.t | Ans of string | Tpl of string | Bbl of (Address.t * Z.t) list

let pp = function
    | Ins(instr) -> Instruction.pp instr
    | Fun(func) -> Function.pp func
    | Src(src) -> Source.pp src
    | Snk(snk) -> Sink.pp snk
    | Ans(s) -> s
    | Tpl(s) -> "instruction template: " ^ s
    | Bbl(l) -> Printf.sprintf "BBL: <%s>" @@ Utils.pp_list (fun (addr, opcode) -> (Z.format "%#x" addr) ^ ":" ^ (Z.format "%#x" opcode)) l

class virtual tracer_handle =
    object (self)
        method virtual next : string
        method virtual regval : string -> Storage.t
        method virtual memval : Address.t -> int -> Storage.t
        method virtual get_gdb : Gdb.session
        method virtual query_gdb : string -> Gdb.Res.result
        method virtual name : string
        method virtual get_pid : int
        method virtual check_errors : unit
        method virtual terminate : unit
        method virtual kill : unit

        method virtual get_stats : string -> Stats.Bundle.t
        method virtual get_runtime : Stats.timer
    end

exception NotExec

class pinstrio_h () =
    let pinstrio_path =
        let out = Unix.open_process_in "which pinstrio.so" in
        try
            let res = input_line out in
            close_in out;
            res
        with End_of_file -> (*raise (Failure "pinstrio not found in path")*) "$PINSTRIO"
    in
    let pipinf, pipoutf, pipopts =
        try
            let filein = File.create "tracer_input" "in" in
            let fileout = File.create "tracer_output" "out" in
            Unix.mkfifo (File.get_path filein) 0o640;
            Unix.mkfifo (File.get_path fileout) 0o640;
            filein, 
            fileout, 
            Printf.sprintf "-in %s -out %s" (File.get_path filein) @@ File.get_path fileout
        with err -> raise (Failure(Printf.sprintf "could not open pipe for tracer (%s)" @@ Printexc.to_string err))
    in
    let cmd =
        "pin -appdebug -t " ^
        pinstrio_path ^
        (
            if !main = ""
            then ""
            else " -main " ^ !main
        ) ^
        " -no-avx512 -binary " ^
        pipopts ^ " " ^
        !more_opts ^ " -- " ^
        (File.get_path @@ File.from_file @@ Options.get_target ()) ^
        " " ^
        !target_args
    in
    let session = Subprocess.create_session "pinstrio" cmd in
    let pipout = File.open_in_chan pipoutf in
    let pipin = File.open_out_chan pipinf in
    let gdb =
        let line = Subprocess.receive session in
        if line = "Application stopped until continued from debugger."
        then
        (
            ignore @@ Subprocess.receive session;
            let line = Subprocess.receive session in
            let targexp = Str.regexp {|target remote :\([0-9]+\)|} in
            try
                ignore @@ Str.search_forward targexp line 0;
                let port = Str.matched_group 1 line in
                let gdb = Gdb.create_session () in
                Gdb.target_remote gdb @@ int_of_string port;
                Gdb.continue gdb;
                if input_line pipout = "Ready to start!"
                then gdb
                else
                (
                    ignore @@ Subprocess.close_session @@ Subprocess.kill session;
                    ignore @@ Gdb.kill gdb;
                    raise (Failure("failed to connect debugger to tracer"))
                )
            with Not_found ->
            (
                ignore @@ Subprocess.close_session @@ Subprocess.kill session;
                raise (Failure("failed to connect debugger to tracer"))
            )
        )
        else 
        (
            ignore @@ Subprocess.close_session @@ Subprocess.kill session;
            raise (Failure(Printf.sprintf "tracing command failed with <%s>" line))
        )
    in
    object (self)
        val mutable session = session
        val pipin = pipin
        val pipout = pipout
        val mutable gdb = gdb
        val mutable gdb_open = false
        val mutable bbl = []
        val mutable synced = true
        val mutable last = Ans("lorem ipsum")
        val mutable funcstate = Function.empty_state
        val stats = TraceStats.create "pinstrio"

        method send cmd =
            Printf.fprintf pipin "%s\n" cmd;
            flush pipin

        method receive =
            input_line pipout

        method get_trace ?(update = true) () =
            let rec get_trace () =
                let line = self#receive in
                stats.runtime#stop;
                stats.tracerruntime#stop;
                stats.parsing#start;
                let receive_bin data size =
                    let data = ref data in
                    while String.length !data < size;
                    do
                        data := !data ^ "\n" ^ self#receive;
                    done;
                    !data
                in
                try
                    assert (String.length line > 0);
                    let trace = String.sub line 1 ((String.length line) - 1) in
                    let res = 
                        match String.get line 0 with
                        | '0' -> 
                        (
                            let func, nfuncstate = Function.parse trace funcstate in
                            if update then funcstate <- nfuncstate;
                            Fun(func)
                        )
                        | '1' -> 
                        (
                            let data = receive_bin trace 16 in
                            let addr = Z.of_bits @@ String.sub data 0 8 in
                            let opcodesize = Z.to_int @@ Z.of_bits @@ String.sub data 8 8 in
                            let data = receive_bin data (opcodesize + 41) in
                            let opcode = Z.of_bits @@ String.concat "" @@ List.init opcodesize (fun i -> String.sub data (16 + opcodesize - i - 1) 1) in
                            let flags = Bytes.get_uint8 (Bytes.of_string @@ String.sub data (16 + opcodesize) 1) 0 in
                            let branch = 
                                match flags land 0b11 with
                                | 0 -> None
                                | 0b10 -> Some(false)
                                | 0b11 -> Some(true)
                                | _ -> assert false
                            in
                            let read1 = Z.of_bits @@ String.sub data (opcodesize + 17) 8 in
                            let read2 = Z.of_bits @@ String.sub data (opcodesize + 25) 8 in
                            let write = Z.of_bits @@ String.sub data (opcodesize + 33) 8 in
                            match flags land 0b1100 with
                            | 0b1000 -> raise NotExec
                            | 0b1100 | 0b0100 -> 
                            (
                                if not @@ Instruction.has_template opcode
                                then
                                (
                                    stats.parsing#stop;
                                    let tr = self#query "t" in
                                    stats.parsing#start;
                                    match tr with
                                    | Tpl(_) -> ()
                                    | _ -> assert false
                                )
                                ;
                                Ins(Instruction.create (Function.get_toplevel funcstate) addr opcode branch read1 read2 write)
                            )
                            | _ -> assert false
                        )
                        | '2' -> Src(Source.parse trace)
                        | '3' -> Snk(Sink.parse trace)
                        | '4' -> Ans(trace)
                        | '5' -> 
                        (
                            Instruction.add_template trace;
                            stats.instrcache#incr;
                            Tpl(trace)
                        )
                        | '6' ->
                        (
                            let data = receive_bin trace 8 in
                            let size = Z.to_int @@ Z.of_bits @@ String.sub data 0 8 in
                            let _, _, instrs = List.fold_left
                                (
                                    fun (data, size, res) _ ->
                                        let data = receive_bin data (size + 16) in
                                        let addr = Z.of_bits @@ String.sub data size 8 in
                                        let opcodesize = Z.to_int @@ Z.of_bits @@ String.sub data (size + 8) 8 in
                                        let data = receive_bin data (size + 16 + opcodesize) in
                                        let opcode = Z.of_bits @@ String.concat "" @@ List.init opcodesize (fun i -> String.sub data (size + 16 + opcodesize - i - 1) 1) in
                                        let size = size + 16 + opcodesize in
                                        data, size, (addr, opcode)::res
                                )
                                (data, 8, [])
                                @@ List.init size (fun i -> i)
                            in
                            Bbl(List.rev instrs)
                        )
                        | _ -> raise (Failure(Printf.sprintf "unknown trace id <%c>" @@ String.get line 0))
                    in
                    stats.parsing#stop;
                    res
                with 
                | NotExec -> 
                (
                    stats.parsing#stop;
                    raise NotExec
                )
                | err ->
                (
                    stats.parsing#stop;
                    raise (Failure(Printf.sprintf "failed to parse trace <%s> (%s)" line @@ Printexc.to_string err))
                )
            in
            get_trace()

        method sync =
            if not synced
            then
            (
                match bbl with
                | (addr, _)::_ -> 
                (
                    try
                        last <- self#query ("g " ^ Address.to_string_hex addr);
                        stats.synccnt#incr;
                        synced <- true
                    with _ -> assert false
                )
                | _ -> ()
            )

        method maybe_open_gdb =
            if not gdb_open
            then
            (
                self#sync;
                gdb_open <- true;
                self#send "d"
            )

        method maybe_close_gdb =
            if gdb_open
            then
            (
                gdb_open <- false;
                Gdb.continue gdb;
                (*Sometimes the function name is resolved after gdb query*)
                let lastexp = 
                    let toks = Str.split (Str.regexp_string " *invalid* ") @@ pp last in
                    Str.regexp @@ List.fold_left (fun expr tok -> expr ^ {| [^ ]+ |} ^ (Str.quote tok)) (Str.quote @@ List.hd toks) @@ List.tl toks
                in
                let cnt = ref 0 in
                let ok tr =
                    match tr, last with
                    | Fun(CallRet(id1, _)), Fun(CallRet(id2, _)) -> id1 = id2
                    | Fun(_), Fun(_)
                    | Ins(_), Ins(_) ->
                    (
                        cnt := !cnt + 1;
                        Str.string_match lastexp (pp tr) 0
                    )
                    | _ -> tr = last
                in
                while not @@ ok @@ self#get_trace ~update:false ();
                do
                    if !cnt > 10 then raise (Failure "failed to recover from GDB query");
                    self#send ""
                done
            )

        method query q =
            self#maybe_close_gdb;
            stats.queries#incr;
            if q = "" || q.[0] = 'g' then stats.runtime#start;
            stats.tracerruntime#start;
            self#send q;
            try
                let res = self#get_trace () in
                Message.Wrap.send (Wrap.Trace(lazy (pp res)));
                match res with
                | Ans("ERROR") -> raise (Failure (Printf.sprintf "<%s>: error" q))
                | Ans("SIGSEGV") -> raise End_of_file
                | _ -> res
            with End_of_file -> 
            (
                stats.runtime#stop;
                stats.tracerruntime#stop;
                Message.to_main @@ Message.Wrap.mk_message Wrap.Crash;
                (*should not return*)
                assert false
            )

        method get_gdb =
            self#maybe_open_gdb;
            gdb

        method query_gdb q =
            self#maybe_open_gdb;
            Gdb.query gdb q

        method next =
            let rec do_next () =
                let rec aux () =
                    match bbl with
                    | (addr, opcode)::t ->
                    (
                        let aux_ () =
                            try
                                last <- self#query ("g " ^ Address.to_string_hex addr);
                                stats.synccnt#incr;
                                synced <- true;
                                last
                            with NotExec ->
                            (
                                match t with
                                | [] -> do_next ()
                                | _ ->
                                (
                                    bbl <- t;
                                    synced <- false;
                                    aux ()
                                )
                            )
                        in
                            
                        if not @@ Instruction.has_template opcode 
                        then aux_ ()
                        else
                        (
                            match Instruction.create_from_nothing (Function.get_toplevel funcstate) addr opcode with
                            | Some(instr) -> 
                            (
                                let tr = Ins(instr) in
                                stats.asynccnt#incr;
                                Message.Wrap.send (Wrap.Trace(lazy (pp tr)));
                                tr
                            )
                            | _ -> aux_ ()
                        )
                    )
                    | _ -> assert false
                in
                match bbl with
                | [_] | [] ->
                (
                    last <- self#query "";
                    match last with
                    | Bbl(instrs) ->
                    (
                        bbl <- instrs;
                        synced <- false;
                        aux ()
                    )
                    | _ -> 
                    (
                        bbl <- [];
                        synced <- true;
                        last
                    )
                )
                | _::t -> 
                (
                    bbl <- t;
                    synced <- false;
                    aux ()
                )
            in
            do_next ()

        method parse_value = function
            | Ans(s) ->
            (
                let rec aux s =
                    let byte s = 
                        String.sub s 0 2 
                    in
                    match String.length s with
                    | 0 -> raise (Failure "empty value")
                    | 1 -> raise (Failure "odd value length")
                    | 2 -> "0x" ^ s
                    | n -> (aux (String.sub s 2 (n - 2))) ^ (byte s)
                in
                Z.of_string @@ aux s
            )
            | _ -> assert false

        method regval reg =
            self#sync;
            let q = Printf.sprintf "v REG<%s>" reg in
            try
                self#parse_value @@ self#query q
            with e -> raise (Failure (Printf.sprintf "cannot get value for <%s> (%s)" q (Printexc.to_string e)))

        method memval addr size =
            self#sync;
            let q = Printf.sprintf "v MEM<%s>[%d]" (Address.to_string_hex addr) size in
            try
                self#parse_value @@ self#query q
            with e -> raise (Failure (Printf.sprintf "cannot get value for <%s> (%s)" q (Printexc.to_string e)))

        method name =
            "Pinstrio"

        method get_pid =
            try
                let ans = 
                    match self#query "p" with
                    | Ans(s) -> s
                    | _ -> assert false
                in
                int_of_string ans
            with e ->
            (
                Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "could not get pid: %s" @@ Printexc.to_string e)));
                -1
            )

        method check_errors = 
            (*Subprocess.receive_all_errs session*)
            ()

        method terminate =
            ignore @@ self#query "q";
            session <- Subprocess.close_session session;
            gdb <- Gdb.close_session gdb;
            close_in pipout;
            close_out pipin

        method kill =
            ignore @@ Gdb.kill gdb;
            ignore @@ Subprocess.close_session @@ Subprocess.kill session;
            close_in pipout;
            close_out pipin

        method get_stats name =
            TraceStats.to_bundle name stats
                |> Stats.Bundle.add_stat (gdb.qcnt :> Stats.stat)
                |> Stats.Bundle.add_stat (gdb.timer :> Stats.stat)

        method get_runtime =
            Oo.copy stats.runtime
    end

let h = ref None

let stop = ref false

let timer = new Stats.timer "" ""

let start () =
    let tr =
        match !h with
        | Some(_) -> raise (Failure "tracer already running")
        | None -> new pinstrio_h ()
    in
    Stats.register ~big:false @@ tr#get_stats "Tracing";
    h := Some(tr);
    timer#start

let get_running () =
    match !h with
    | None -> raise End_of_file
    | Some(h) -> h

let regval reg =
    (get_running ())#regval reg

let memval addr size =
    (get_running ())#memval addr size

let stoval sto =
    match sto with
    | Storage.Register(reg, _) -> regval reg
    | Memory(addr, size) -> memval addr size
    | Custom(_, _, value) -> value

let rec exprval = function
    | Expr.Const(z, _) -> z
    | Var(sto) -> stoval sto
    | Unop(Restrict(b, e), a) -> Z.extract (exprval a) (b * 8) ((e - b + 1) * 8)
    | Unop(Extend(s), a) -> exprval a
    | Unop(Strlen(s), _) -> s
    | Bnop(Add, a, b) -> Z.add (exprval a) @@ exprval b
    | Bnop(Sub, a, b) -> Z.sub (exprval a) @@ exprval b
    | Bnop(Mul, a, b) -> Z.mul (exprval a) @@ exprval b
    | Bnop(Max, a, b) -> max (exprval a) @@ exprval b
    | Bnop(Min, a, b) -> min (exprval a) @@ exprval b
    | Bnop(Dist, a, b) -> Z.abs @@ Z.sub (exprval a) @@ exprval b

let get_gdb () =
    (get_running ())#get_gdb

let query_gdb cmd =
    (get_running ())#query_gdb cmd

let check_errors () =
    (get_running ())#check_errors

let terminate () =
    (get_running ())#terminate;
    h := None

let terminate_next () =
    stop := true

let terminated () =
    !stop

let kill () =
    (get_running ())#kill;
    h := None

let next () =
    if !stop
    then (terminate (); Ans("DONE"))
    else if !timeout > 0 && timer#get > Float.of_int !timeout
    then 
    (
        terminate ();
        Message.to_main @@ Message.Wrap.mk_message Wrap.Timeout;
        (*should not return*)
        assert false
    )
    else (get_running ())#next

let get_runtime () =
    (get_running ())#get_runtime

let get_pid () =
    (get_running ())#get_pid

let get_main_function () =
    !main

let _ =
    Options.register_option ("-tracer-opts", Arg.Set_string more_opts, "Tracer additional options.");
    Options.register_option ("-args", Arg.Set_string target_args, "Arguments to pass to the target.");
    Options.register_option ("-main", Arg.Set_string main, "Main function where tracing should start.");
    Options.register_option ("-timeout", Arg.Set_int timeout, "Timeout (seconds).");
    Options.register_alias "-timeout" "-t"
