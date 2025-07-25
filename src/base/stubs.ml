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

module type Base =
    sig
        val modulename : string

        type state
        type action
        type kind

        val pp_kind : kind -> string
        val check_kind : Function.func -> Function.CallId.t -> state -> kind -> state * bool
        val act : Function.func -> Function.CallId.t -> state -> action -> state
    end

module type Sig =
    sig
        include Base

        type stub = Function.func -> Function.CallId.t -> state -> action
        type parsr = string list -> stub

        type t

        val register : string -> string list -> string -> kind -> parsr -> unit
        val register_default : string -> string -> kind -> stub -> unit

        val get_parsr : string -> kind * parsr

        val pp : t -> string

        val create : string -> t

        val run : t -> Function.t -> state -> state

        val cli_specs : string Options.TaggedOptions.opt

        val register_options : string -> unit

        val basic_parsr : stub -> parsr
    end

module Make (B : Base) =
    struct
        include B

        type stub = Function.func -> Function.CallId.t -> state -> action
        type stub_entry = string * kind * stub
        type parsr = string list -> stub
        type parsr_entry = string list * string * kind * parsr

        type t = stub_entry list Utils.StringMap.t

        let parsers = ref Utils.StringMap.empty

        let register name argdesc desc (kind : kind) (parsr : parsr) =
            try
                ignore @@ Utils.StringMap.find name !parsers;
                raise (Failure (Printf.sprintf "%s stub name <%s> already in use" modulename name))
            with Not_found -> parsers := Utils.StringMap.add name (argdesc, desc, kind, parsr) !parsers

        let list_available () =
            let s = Utils.StringMap.fold (fun name (argdesc, desc, _, _) res -> res ^ (Printf.sprintf "- %s(%s):\n\t%s\n" name (Utils.pp_list (fun e -> e) argdesc) desc)) !parsers @@ Printf.sprintf "Available %s stubs:\n" modulename in
            Message.Wrap.send (Message.Wrap.BigInfo(s))

        let default = ref Utils.StringMap.empty

        let register_default func name kind stub =
            try
                let l = Utils.StringMap.find func !default in
                default := Utils.StringMap.add func ((name, kind, stub)::l) !default
            with Not_found -> default := Utils.StringMap.add func [name, kind, stub] !default

        let get_parsr name =
            let _, _, kind, parsr = Utils.StringMap.find name !parsers in
            kind, parsr

        let pp stubs =
            let pp = fun (name, _, _) -> name in
            Utils.StringMap.fold (fun func l res -> res ^ (Printf.sprintf "- %s -> %s\n" func @@ Utils.pp_list pp l)) !default ""

        let list_default () =
            let s = Printf.sprintf "default %s stub attributions:\n%s" modulename @@ pp !default in
            Message.Wrap.send (Message.Wrap.BigInfo(s))

        let parse_stub s =
            let exp = Str.regexp {|\([^(]+\)\((\(.*\))\)?$|} in
            if Str.string_match exp s 0
            then
            (
                let name = Str.matched_group 1 s in
                let args = 
                    try
                        match String.split_on_char ',' @@ Str.matched_group 3 s with
                        | [""] -> []
                        | l -> l
                    with Not_found -> []
                in
                try
                    let argdesc, _, kind, parsr = Utils.StringMap.find name !parsers in
                    if not (List.length argdesc = List.length args)
                    then raise (Failure ("invalid number of arguments"))
                    ;
                    name, kind, parsr args
                with 
                | Not_found -> raise (Failure(Printf.sprintf "no %s stub named <%s>" modulename name))
                | e -> raise (Failure(Printf.sprintf "error while parsing %s stub specification <%s> (%s)" modulename s @@ Printexc.to_string e))
            )
            else raise (Failure (Printf.sprintf "invalid %s stub format <%s>" modulename s))

        let parse_attributions s stubs =
            let l = List.map (String.split_on_char ':') @@ String.split_on_char ';' s in
            let rec do_parse stubs = function
                | [func]::t
                | [func; ""]::t -> do_parse (Utils.StringMap.add func [] stubs) t
                | [func; stub]::t -> 
                (
                    let entry = parse_stub stub in
                    let stubs =
                        try
                            let l = Utils.StringMap.find func stubs in
                            Utils.StringMap.add func (entry::l) stubs
                        with Not_found -> Utils.StringMap.add func [entry] stubs
                    in
                    do_parse stubs t
                )
                | l::_ -> raise (Failure (Printf.sprintf "%s stub parsing of <%s> failed: invalid formatting" modulename @@ Utils.pp_list ~sep:":" (fun e -> e) l))
                | _ -> stubs
            in
            do_parse stubs l

        let create s =
            parse_attributions s !default

        let run stubs func a =
            match func with
            | Function.Entry(func) ->
            (
                match func.Function.call_id with
                | Some(call_id) ->
                (
                    let stubs = 
                        try
                            Utils.StringMap.find func.Function.fname stubs 
                        with Not_found -> []
                    in
                    let do_run a (name, kind, stub) =
                        let a, ok = check_kind func call_id a kind in
                        if ok
                        then
                        (
                            Message.Wrap.send (Message.Wrap.Debug("STUB", lazy (Printf.sprintf "running %s stub <%s> (%s) for function <%s>" modulename name (pp_kind kind) func.fname)));
                            act func call_id a @@ stub func call_id a
                        )
                        else
                        (
                            Message.Wrap.send (Message.Wrap.Debug("STUB", lazy (Printf.sprintf "suppressed %s stub <%s> (%s) for function <%s>" modulename name (pp_kind kind) func.fname)));
                            a
                        )
                    in
                    List.fold_left do_run a stubs
                )
                | _ -> a
            )
            | _ -> a

        let cli_specs = new Options.TaggedOptions.opt ""
            
        let register_options policy =
            Options.register_policy_option policy (Printf.sprintf "-list-%s-stubs" modulename) (Options.TaggedOptions.Untagged(fun () -> Options.register_todo list_available)) (Printf.sprintf "List available %s stubs." modulename);
            Options.register_policy_option policy (Printf.sprintf "-list-default-%s-stubs" modulename) (Options.TaggedOptions.Untagged(fun () -> Options.register_todo list_default)) (Printf.sprintf "List default %s stubs." modulename);
            Options.register_policy_option policy (Printf.sprintf "-%s-stubs" modulename) (Options.TaggedOptions.String(cli_specs)) (Printf.sprintf "Set %s stubs (format: function_name:stub_name(arg1,...);...) (no arguments: function_name:stub_name;...) (clear: function_name:;...)." modulename)

        let basic_parsr stub =
            fun _ -> stub
    end

module GdbArgs =
    struct
        let argexp = Str.regexp {|@\([0-9]+\)|}
        let retexp = Str.regexp {|@ret|}
        let exp = Str.regexp {|retval$\|[0-9]+$\|\(ret\|strlen\|gdb\)<\(.+\)>$|}

        type t = Arg of int | RetVal | Gdb of string | Ret of t | Strlen of t

        let spec = "[0-9]+ (argnum) | retval | gdb<gdb expr> | ret<expr> (evaluated before return) | strlen<expr>, with @[0-9]+ and @ret for arguments and the return value in gdb expressions"

        let rec parse s =
            if Str.string_match exp s 0
            then
            (
                if s = "retval"
                then RetVal
                else if String.ends_with ~suffix:">" s
                then
                (
                    let cmd = Str.matched_group 1 s in
                    let expr = Str.matched_group 2 s in
                    match cmd with
                    | "ret" -> Ret(parse expr)
                    | "strlen" -> Strlen(parse expr)
                    | "gdb" -> Gdb(expr)
                    | _ -> assert false
                )
                else Arg(int_of_string s)
            )
            else raise (Failure(Printf.sprintf "invalid gdb stub arg <%s>" s))

        let get func expr =
            let expr =
                let arg_replacer s =
                    let arg = Str.matched_string s in
                    let argnum = int_of_string @@ String.sub arg 1 ((String.length arg) - 1) in
                    Z.to_string @@ Trace.stoval @@ Function.get_iarg argnum func
                in
                let rec aux = function
                    | Gdb(expr) -> Gdb(Str.global_substitute argexp arg_replacer expr)
                    | Ret(expr) -> Ret(aux expr)
                    | Strlen(expr) -> Strlen(aux expr)
                    | expr -> expr
                in
                aux expr
            in
            let eval_gdb expr =
                let ret = Z.to_string @@ Trace.stoval @@ Function.get_iret 0 func in
                let gdb = Trace.get_gdb () in
                Trace.Gdb.eval_expr gdb @@ Str.global_replace retexp ret expr
            in
            let rec aux = function
                | Arg(n) ->
                (
                    let value = Trace.stoval @@ Function.get_iarg n func in
                    lazy value
                )
                | RetVal -> lazy (Trace.stoval @@ Function.get_iret 0 func)
                | Ret(expr) -> lazy (Lazy.force @@ aux expr)
                | Gdb(expr) ->
                (
                    let value = 
                        match eval_gdb expr with
                        | Some(value) -> value
                        | _ -> raise (Failure(Printf.sprintf "gdb failed to evaluate <%s>" expr))
                    in
                    lazy value
                )
                | Strlen(expr) -> 
                (
                    let length = Analysis.Utils.strlen @@ Lazy.force @@ aux expr in
                    lazy length
                )
            in
            aux expr
    end
