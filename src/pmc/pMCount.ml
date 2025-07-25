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

module PMCStats =
    struct
        let runtime = new Stats.timer "runtime" "Total runtime"
        let succ = new Stats.counter "succ" "Success" 0
        let timeout = new Stats.counter "timeout" "Timeouts" 0
        let err = new Stats.counter "err" "Errors" 0

        let b =
            Stats.Bundle.create "Projected Model Counting"
                |> Stats.Bundle.add_stat (succ :> Stats.stat)
                |> Stats.Bundle.add_stat (timeout :> Stats.stat)
                |> Stats.Bundle.add_stat (err :> Stats.stat)
                |> Stats.Bundle.add_stat (runtime :> Stats.stat)

        let registered = ref false

        let register () =
            if not !registered
            then
            (
                Stats.register ~big:false b;
                registered := true
            )
    end

module CountVal =
    struct
        type t = Count of Z.t | Timeout | Error

        let to_generic = function
            | Count(c) -> Result.Int(c)
            | Timeout -> Result.Str("timeout")
            | Error -> Result.Str("error")
    end

module Count = Result.Make (CountVal)

module StringMap = Map.Make(String)

module Solver =
    struct
        let selected = ref "d4"
        let timeout = ref (-1)

        type t =
            {
                desc : string;
                mk_cmd : projections:(string list) -> File.t -> string;
                parse_out : string list * string list * Count.result -> Count.result
            }

        let solvers = ref StringMap.empty

        let register name desc mk_cmd parse_out =
            solvers := StringMap.add name {desc; mk_cmd; parse_out} !solvers

        let list_solvers () =
            let msg = StringMap.fold (fun name e s -> s ^ "\n- " ^ name ^ ": " ^ e.desc) !solvers "Available PMC solvers:" in
            Message.Wrap.send (Message.Wrap.BigInfo(msg))

        let do_query solver (cmd, r) =
            PMCStats.register ();
            let timer = new Stats.timer "runtime" "Runtime" in
            let r = Count.add_stat (timer :> Stats.stat) r in
            PMCStats.runtime#start;
            timer#start;
            let session = Subprocess.create_session solver cmd in
            let res = Subprocess.receive_all session in
            let err = Subprocess.log_errors ~big:false solver session in
            timer#stop;
            PMCStats.runtime#stop;
            ignore @@ Subprocess.close_session session;
            res, err, r

        let query ?(solver = "") ~projections file =
            if projections = []
            then
            (
                Message.Wrap.send (Message.Wrap.BigWarning(lazy ("pmc: empty projection set")));
                raise (Failure "empty projection set")
            )
            ;
            let todo = 
                if solver = ""
                then String.split_on_char ';' !selected
                else [solver]
            in
            let query solver =
                try
                    let s = StringMap.find solver !solvers in
                    let label = solver in
                    let res = ((s.mk_cmd ~projections file), Count.create ~label Error)
                        |> do_query solver
                        |> s.parse_out
                    in
                    (
                        match res.Result.result with
                        | Count(_) -> PMCStats.succ#incr
                        | Timeout -> PMCStats.timeout#incr
                        | Error -> PMCStats.err#incr
                    )
                    ;
                    res
                with Not_found -> raise (Failure (Printf.sprintf "unknown mc solver %s" solver))
            in
            List.map query todo

        let _ =
            Options.register_option ("-pmc-solver", Arg.Set_string selected, "Solvers for projected model counting (format: solver1;solver2;...).");
            Options.register_option ("-pmcs", Arg.Set_string selected, "Alias for -pmc-solver.");
            Options.register_option ("-pmc-timeout", Arg.Set_int timeout, "Projected model counting timeout (ms).");
            Options.register_option ("-pmct", Arg.Set_int timeout, "Alias for -pmc-timeout.");
            Options.register_option ("-list-pmc-solvers", Arg.Unit(fun () -> Options.register_todo list_solvers), "List available PMC solvers.")
    end

module Popcon =
    struct
        let compat projections fml =
            (*let apply_cond = fun cond f arg -> if cond then f arg else arg in*)
            (*let rec find_all exp extract s n =
                try
                    if n >= (String.length s) then raise Not_found;
                    let next = Str.search_forward exp s n in
                    let e = extract s in
                    e::(find_all exp extract s (next + 1))
                with Not_found -> []
            in*)
            (*disgusting code ahead*)
            let fix_lines s =
                let indentexp = Str.regexp {|^[ ]*|} in
                let cnt = ref 0 in
                let aux c =
                    match !cnt, c with
                    | _, '(' -> 
                    (
                        cnt := !cnt + 1;
                        c
                    )
                    | _, ')' -> 
                    (
                        cnt := !cnt - 1;
                        c
                    )
                    | 0, ' ' -> '\n'
                    | 0, _ -> c
                    | _, '\n' -> ' '
                    | _, _ -> c
                in
                String.map aux @@ Str.global_replace indentexp "" s
            in
            let fix_litterals s =
                let littexp = Str.regexp {|#\([xb]\)\([0]*\)\([a-zA-Z0-9]+\)|} in
                let replacer s =
                    let base = Str.matched_group 1 s in
                    let leadz = Str.matched_group 2 s in
                    let ns = Str.matched_group 3 s in
                    let n =
                        if (String.length ns) = 0
                        then Z.zero
                        else Z.of_string ("0" ^ base ^ ns) 
                    in
                    let size =
                        let charsize = (String.length leadz) + (String.length ns) in
                        match base with
                        | "x" -> 4 * charsize
                        | "b" -> charsize
                        | _ -> assert false
                    in
                    Printf.sprintf "(_ bv%s %d)"
                        (Z.to_string n)
                        size
                in
                Str.global_substitute littexp replacer s
            in
            let const_to_fun s =
                let constexp = Str.regexp {|declare-const \([^ ]+\)|} in
                let replacer s =
                    Printf.sprintf "declare-fun %s ()" (Str.matched_group 1 s)
                in
                Str.global_substitute constexp replacer s
            in
            let fix_proj s =
                let projexp = Str.regexp {|(define-fun \([^ ]+\) () \(([^)]+)\) \(.+$\)|} in
                let to_assert = ref [] in
                let replacer s =
                    let name = Str.matched_group 1 s in
                    try
                        ignore @@ List.find (fun e -> e = name) projections;
                        let typ = Str.matched_group 2 s in
                        let rval = Str.matched_group 3 s in
                        to_assert := (name, rval)::!to_assert;
                        Printf.sprintf "(declare-fun %s () %s)" name typ
                    with Not_found -> Str.matched_string s
                in
                let res = Str.global_substitute projexp replacer s in
                let add_assert res (lval, rval) =
                    res ^
                    (Printf.sprintf "(assert (= %s %s)" lval rval)
                in
                List.fold_left add_assert res !to_assert
            in
            let fix_names s =
                let cnt = ref 0 in
                let position = ref 0 in
                let exp = Str.regexp {|\(declare\|define\)[^ ]+ \([^ ]+\)|} in
                let rec aux s =
                    try
                        position := (Str.search_forward exp s !position) + 1;
                        let name = Str.matched_group 2 s in
                        try
                            (*don't fix projection names*)
                            ignore @@ List.find (fun e -> e = name) projections;
                            s
                        with Not_found ->
                        (
                            let varexp = Str.regexp ({|\([ (]\)|} ^ (Str.quote name) ^ {|\([ )]\)|}) in
                            let replacer s =
                                let first = Str.matched_group 1 s in
                                let last = Str.matched_group 2 s in
                                Printf.sprintf "%svar_%d%s" first !cnt last
                            in
                            let res = Str.global_substitute varexp replacer s in
                            (*in case of xxx name name*)
                            let res = Str.global_substitute varexp replacer res in
                            cnt := !cnt + 1;
                            aux res
                        )
                    with Not_found -> s
                in
                aux s
            in
            (*let blast_arrays s =
                let file = File.create "array_blasting" ".smt2" in
                File.write s file;
                let cmd = "binsec-x86_64-cav.AppImage -fml -fml-blast-arrays " ^ (File.get_path file) in
                let session = Subprocess.create_session "array blasting" cmd in
                ignore @@ Subprocess.log_errors "array blasting" session;
                let session = Subprocess.close_session session in
                let res = File.from_file "smt_simpl_out.smt2" in
                let lines = File.read res in
                File.delete res;
                List.fold_left (fun res e -> res ^ "\n" ^ e) "" lines
            in*)
            let add_proj_info s =
                let projexp = Str.regexp {|(declare-fun \([^ ]+\).+$|} in
                let replacer s =
                    let l = Str.matched_string s in
                    let name = Str.matched_group 1 s in
                    try
                        ignore @@ List.find (fun e -> e = name) projections;
                        Printf.sprintf "%s\n(set-info :projected %s)" l name
                    with Not_found -> Str.matched_string s
                in
                Str.global_substitute projexp replacer s
            in
            fml         
                |> fix_lines
                |> fix_names
                |> fix_litterals
                |> const_to_fun
                |> fix_proj
                |> add_proj_info

        let do_compat projections file =
            let fml = compat projections @@ List.fold_left (fun res e -> res ^ e ^ "\n") "" @@ File.read file in
            File.write fml file

        let mk_cmd ?(debug = false) ?(timeout = !Solver.timeout) ~projections file =
            do_compat projections file;
            Printf.sprintf "popcon %s %s -g ProjectedModelCount %s"
                (
                    if timeout > 0
                    then Printf.sprintf "-T %d" timeout
                    else ""
                )
                (
                    if debug
                    then "-d"
                    else ""
                )
                @@ File.get_path file

        let get_cnf ~projections file =
            let res, _, _ = Solver.do_query "popcon" ((mk_cmd ~debug:true ~timeout:1000 ~projections file), Count.create CountVal.Error) in
            let files = ref [] in
            let exp = Str.regexp {|.*Persisting temporary data to \(.*$\)|} in
            List.iter
                (
                    fun line ->
                    (
                        if Str.string_match exp line 0
                        then files := (File.from_file (Str.matched_group 1 line))::!files
                    )
                )
                res
            ;
            match !files with
            | proj::cnf::_ ->
            (
                let projs = String.split_on_char ',' @@ List.hd @@ File.read proj in
                let cnfs = File.read cnf in
                projs, cnfs
            )
            | _ -> raise (Failure "could not find cnf")

        let parse_out (out, _, r) =
            let resexp = Str.regexp {|Models: \([0-9]+\), Influence \([0-9.]+\), Total bits: \([0-9]+\), Incidence \(-?[0-9.]+\)|} in
            let timeoutexp = Str.regexp "timeout=true" in
            let rec check_out l =
                match l with
                | line::t ->
                (
                    try
                        ignore @@ Str.search_forward resexp line 0;
                        let count = Str.matched_group 1 line |> Z.of_string in
                        {r with Result.result = CountVal.Count(count)}
                    with Not_found ->
                    (
                        try
                            ignore (Str.search_forward timeoutexp line 0);
                            Message.Wrap.send (Message.Wrap.BigWarning((lazy ("d4: timeout"))));
                            {r with result = CountVal.Timeout}
                        with Not_found -> check_out t
                    )
                )
                | _ -> {r with result = CountVal.Error}
            in
            check_out out

        let _ =
            Solver.register "d4" "Exact model counter." (fun ~projections file -> mk_cmd ~projections file) parse_out
    end
