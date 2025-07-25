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

let verbosity = ref 1
let backlog = ref false
let backlog_size = ref 1000
let outfile = ref None

let create_outfile fname =
    outfile := Some(File.from_file ~create_dir:true fname)

type ext_value = ..

type ext_value += Value of string Lazy.t
type ext_value += Uncaught of string * string
type ext_value += Clean

let err_cnt = ref 0

type t = 
    {
        verbosity : int;
        error : int option;
        ext_value : ext_value;
        value : string Lazy.t;
        prefix : string;
        handled_by : Identifier.t list
    }

type signal = ..

let is_error msg =
    match msg.error with
    | Some(_) -> true
    | _ -> false

let get_prefix ?(basic = false) msg =
    let prefix =
        let prefix =
            if msg.prefix = ""
            then "COLORSTREAMS"
            else msg.prefix
        in
        List.fold_right (fun e res -> (Identifier.pp e) ^ (if basic then "_" else " → ") ^ res) msg.handled_by prefix
    in
    match msg.error with
    | Some(n) -> Printf.sprintf "%s <%d>" prefix n
    | _ -> prefix

let get_sender msg =
    List.hd @@ List.rev msg.handled_by

class handler ?(name = None) () =
    object (self)
        val name = name
        val mutable signals : signal list = []

        method handle msg =
            match name with
            | Some(name) -> Some({msg with handled_by = name::msg.handled_by})
            | _ -> Some(msg)

        method receive_signal signal =
            signals <- signal::signals

        method get_signals =
            let res = List.rev signals in
            signals <- [];
            res
    end

module Colors =
    struct
        let bla = "\x1B[30m"
        let red = "\x1B[31m"
        let gre = "\x1B[32m"
        let yel = "\x1B[33m"
        let blu = "\x1B[34m"
        let mag = "\x1B[35m"
        let cya = "\x1B[36m"
        let whi = "\x1B[37m"
        let gra = "\x1B[90m"

        let base = "\x1B[0m"

        let print_colors = Unix.isatty Unix.stdout
        let print_colors_err = Unix.isatty Unix.stderr

        let get prio err =
            if err && print_colors_err
            then
            (
                let col = if prio = 0 then red else base in
                red, col, base
            )
            else if print_colors
            then
            (
                let col1 =
                    match prio with
                    | 0 -> yel
                    | 1 -> cya
                    | 2 -> gre
                    | 3 -> blu
                    | _ -> mag
                in
                let col2 = if prio > 3 then gra else base in
                col1, col2, base
            )
            else "", "", ""
    end

module Backlog =
    struct
        type t = 
            {
                b : string Lazy.t array;
                size : int;
                i : int
            }

        let create n =
            {b = Array.make n (lazy ""); size = n; i = 0}

        let log msg bl =
            let s = lazy
                (
                    let smsg = Lazy.force msg.value in
                    let prefix = get_prefix msg in
                    let maybe_ret =
                        if String.contains smsg '\n'
                        then "\n"
                        else " "
                    in
                    let s = Printf.sprintf "[%s]%s%s" prefix maybe_ret smsg in
                    if String.ends_with ~suffix:"\n" smsg then String.sub s 0 ((String.length s) - 1) else s
                )
            in
            Array.set bl.b bl.i s;
            {bl with i = (bl.i + 1) mod bl.size}

        let out ?(name = "error") bl =
            let file = File.create ~temporary:false name ".log" in
            let rec aux first n =
                if first || n <> bl.i
                then 
                (
                    let line =
                        try
                            Lazy.force @@ Array.get bl.b n
                        with Lazy.Undefined -> "<line missing; bricked by interrupt?>"
                    in
                    line ^ "\n" ^ (aux false ((n + 1) mod bl.size))
                )
                else ""
            in
            File.write (aux true bl.i) file;
            file
    end

class backlogged_handler ?(name = None) () =
    object (self)
        inherit handler ~name () as super

        val mutable backlog = 
            if !backlog 
            then Some(Backlog.create !backlog_size) 
            else None

        method handle msg =
            let _ =
                match backlog with
                | Some(bl) ->
                (
                    let bl = Backlog.log msg bl in
                    backlog <- Some(bl);
                    if (is_error msg) && msg.verbosity <= !verbosity 
                    then
                    (
                        let name =
                            match name with
                            | Some(name) -> (Identifier.pp_basic name) ^ ":" ^ (get_prefix ~basic:true msg)
                            | _ -> get_prefix ~basic:true msg
                        in
                        ignore @@ Backlog.out ~name bl
                    )
                )
                | _ -> ()
            in
            super#handle msg
    end

class logger =
    object (self)
        inherit backlogged_handler () as super

        val mutable warnings = Utils.StringMap.empty

        val out_file_chan =
            match !outfile with
            | Some(file) -> Some(File.open_out_chan file)
            | _ -> None

        method handle msg =
            let check_redundant = lazy
                (
                    let smsg = Lazy.force msg.value in
                    try
                        let cnt = Utils.StringMap.find smsg warnings in
                        warnings <- Utils.StringMap.add smsg (cnt + 1) warnings;
                        true
                    with Not_found ->
                    (
                        warnings <- Utils.StringMap.add smsg 0 warnings;
                        false
                    )
                )
            in
            if msg.verbosity = !verbosity && is_error msg && Lazy.force check_redundant
            then ()
            else if msg.verbosity <= !verbosity
            then 
            (
                let chan = if is_error msg then stderr else stdout in
                let col1, col2, base = Colors.get msg.verbosity @@ is_error msg in
                let smsg = Lazy.force msg.value in
                let prefix = get_prefix msg in
                let maybe_ret =
                    if String.contains smsg '\n'
                    then "\n"
                    else " "
                in
                let maybe_ret_end = if String.ends_with ~suffix:"\n" smsg then "" else "\n" in
                Printf.fprintf chan "%s[%s]%s%s%s%s%s" col1 prefix maybe_ret col2 smsg base maybe_ret_end;
                flush chan;
                match out_file_chan with
                | Some(chan) -> 
                (
                    Printf.fprintf chan "[%s]%s%s%s" prefix maybe_ret smsg maybe_ret_end;
                    flush chan
                )
                | _ -> ()
            )
            ;
            super#handle msg
    end

let handlers = ref []

class cleaner =
    object(self)
        inherit handler () as super

        method handle msg =
            let clean () =
                while not ((Oo.id @@ List.hd !handlers) = Oo.id self)
                do
                    handlers := List.tl !handlers
                done
            in
            match msg.ext_value with
            | Uncaught(_) ->
            (
                clean ();
                super#handle msg
            )
            | Clean ->
            (
                clean ();
                None
            )
            | _ -> super#handle msg
    end

let accept msg =
    let rec aux = function
        | (Some(msg)), h::t -> aux ((h#handle msg), t)
        | None, _ -> ()
        | _ -> assert false
    in
    aux ((Some(msg)), !handlers)

let send_signal signal =
    match !handlers with
    | h::_ -> h#receive_signal signal
    | _ -> assert false

let to_main msg =
    ignore @@ (List.hd @@ List.rev !handlers)#handle msg

let do_run f =
    try
        f () 
    with e -> 
    (
        let verbosity = 0 in
        let error = Some(!err_cnt) in
        err_cnt := !err_cnt + 1;
        let bt = Printexc.get_backtrace () in
        let err = 
            match e with
            | Failure(msg) -> msg
            | _ -> Printexc.to_string e 
        in
        let ext_value = Uncaught(err, bt) in
        let value = 
            if bt = ""
            then lazy (Printf.sprintf "Uncaught exception: %s" err)
            else lazy (Printf.sprintf "Uncaught exception: %s\n%s" err bt) 
        in
        let prefix = "ERROR" in
        let msg = {verbosity; error; ext_value; value; prefix; handled_by = []} in
        accept msg;
        assert false
    )

let with_handler f (h : handler) =
    handlers := h::!handlers;
    let res = do_run f in
    handlers := List.tl !handlers;
    res

let with_handlers f (h : handler list) =
    let old_handlers = !handlers in
    handlers := h @ !handlers;
    let res = do_run f in
    handlers := old_handlers;
    res

let clean () =
    let msg = {verbosity = 5; error = None; ext_value = Clean; value = (lazy "message handler stack cleanup"); prefix = "DEBUG"; handled_by = []} in
    accept msg

module Wrap =
    struct
        type wrap = ..
        type wrap +=
            | BigInfo of string
            | Info of string Lazy.t
            | SmallInfo of string Lazy.t
            | Debug of string * string Lazy.t
            | SmallDebug of string * string Lazy.t
            | BigWarning of string Lazy.t
            | Warning of string Lazy.t

        type unwrapper = (wrap -> int * bool * string * ext_value * string Lazy.t) -> wrap -> int * bool * string * ext_value * string Lazy.t

        let base_unwrapper next = function
            | BigInfo(s) -> 0, false, "INFO", (Value(lazy s)), lazy s
            | Info(s) -> 1, false, "INFO", (Value(s)), s
            | SmallInfo(s) -> 2, false, "INFO", (Value(s)), s
            | Debug(pref, s) -> 4, false, pref, (Value(s)), s
            | SmallDebug(pref, s) -> 5, false, pref, (Value(s)), s
            | BigWarning(s) -> 1, true, "WARNING", (Value(s)), s
            | Warning(s) -> 2, true, "WARNING", (Value(s)), s
            | w -> next w

        let unwrappers = ref [base_unwrapper]

        let do_unwrap = lazy
            (
                List.fold_left (fun res f -> f res) (fun _ -> assert false) !unwrappers 
            )

        let register_unwrapper (uw : unwrapper) =
            unwrappers := uw::!unwrappers

        let unwrap w =
            (Lazy.force do_unwrap) w

        let mk_message w =
            let verbosity, error, prefix, ext_value, value = (Lazy.force do_unwrap) w in
            let error = 
                if error
                then 
                (
                    let err = Some(!err_cnt) in
                    err_cnt := !err_cnt + 1;
                    err
                )
                else None
            in
            {verbosity; error; ext_value; value; prefix; handled_by = []}

        let send w =
            accept @@ mk_message w
    end

let _ =
    Options.register_option 
        (
            "-verbose", 
            Arg.Set_int verbosity, 
            (
                "Output more or less tracing and taint propagation information:\n" ^
                "       |-> 0: results, big infos and fatal errors\n" ^
                "       |-> 1: + warnings, infos and stats (default)\n" ^
                "       |-> 2: + minor warnings and infos\n" ^
                "       |-> 3: + trace\n" ^
                "       |-> 4: + debug\n" ^
                "       |-> 5: ++ debug"
            )
        );
    Options.register_option ("-backlog", Arg.Set backlog, "Enable backlogs on error.");
    Options.register_alias "-backlog" "-bl";
    Options.register_option ("-backlog-size", Arg.Set_int backlog_size, "Set the number of logging messages to backlog (default: 1000).");
    Options.register_alias "-backlog-size" "-bls";
    Options.register_option ("-log", Arg.String(create_outfile), "Log output to given file.");
    Options.register_option ("-exception-backtraces", Arg.Unit(fun () -> Printexc.record_backtrace true), "Enable exception backtrace recording and display.");
    Options.register_alias "-exception-backtraces" "-ebt"
