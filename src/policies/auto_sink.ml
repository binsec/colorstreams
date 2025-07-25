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

let restrict_opt = new Options.TaggedOptions.opt ""

module S =
    struct
        type t =
            {
                restrict : (int * int) Utils.StringMap.t
            }

        let create ?(tag = None) () =
            {restrict = Utils.StringMap.empty}

        let add_stats_to_bundle b _ = b

        let set_restrict restrict s =
            {restrict}
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

let parse_restrict s =
    if s = ""
    then Utils.StringMap.empty
    else
    (
        let exp = Str.regexp {|\([^:]+\):\([0-9]+\)-\([0-9]+\)|} in
        List.fold_left
            (
                fun res e ->
                    try
                        if Str.string_match exp e 0
                        then
                        (
                            let snk = Str.matched_group 1 e in
                            let lo = int_of_string @@ Str.matched_group 2 e in
                            let hi = int_of_string @@ Str.matched_group 3 e in
                            Utils.StringMap.add snk (lo, hi) res
                        )
                        else raise (Failure("invalid spec"))
                    with err -> raise (Failure(Printf.sprintf "invalid sink restriction formatting <%s> (%s)" e @@ Printexc.to_string err))
            )
            Utils.StringMap.empty
            @@ String.split_on_char ';' s
    )

module SinkStubsBase =
    struct
        let modulename = "sink"

        type state = A.t
        type action = Sink of Sink.t | OnRet of string * Sink.t Lazy.t | NextInstr of string * Sink.t Lazy.t | Nothing
        type kind = SnkKind

        let pp_kind _ =
            "sink"

        let check_kind _ _ a _ =
            a, true

        let send_sink snk a =
            let snk =
                try
                    let lo, hi = Utils.StringMap.find (Identifier.pp snk.Sink.name) (A.get_state a).S.restrict in
                    {snk with expr = Expr.Unop(Expr.Restrict(lo, hi), snk.expr)}
                with _ -> snk
            in
            Message.Wrap.send (Analysis.Wrap.Sink(snk));
            A.analyze (Trace.Snk(snk)) a

        let act _ call_id a = function
            | Sink(snk) -> send_sink snk a
            | OnRet(name, snk) -> A.FunctionCallbacks.on_callret call_id name (fun _ a -> send_sink (Lazy.force snk) a) a
            | NextInstr(name, snk) -> A.InstructionCallbacks.add ~priority:999 name 
                (
                    fun _ a ->
                        let a = send_sink (Lazy.force snk) a in
                        A.InstructionCallbacks.remove name a
                )
                a
            | _ -> a
    end

module SinkStubs =
    struct
        include Stubs.Make (SinkStubsBase)

        module DefaultStubs =
            struct
                type Sink.ext_kind += FuncStub of string

                let buf_name = "buf"

                let buf_spec = ["entry / ret"; "ptr"; "size"; "sink name"]

                let buf_desc = "<size> first bytes of <buf> after entry or before return (format: " ^ Stubs.GdbArgs.spec ^ ")"

                let buf point ptr size name func _ _ =
                    let bufexpr = Stubs.GdbArgs.get func ptr in
                    let sizeexpr = Stubs.GdbArgs.get func size in
                    let sto = lazy (Storage.Memory(Lazy.force bufexpr, Z.to_int @@ Lazy.force sizeexpr)) in
                    let kind = Sink.Auto((FuncStub(buf_name)), buf_name) in
                    let snk = lazy (Sink.create ~kind name (Expr.Var(Lazy.force sto))) in
                    if point = "entry"
                    then SinkStubsBase.Sink(Lazy.force snk)
                    else if point = "ret"
                    then SinkStubsBase.OnRet("buf sink", snk)
                    else raise (Invalid_argument(point))

                let buf_parsr = function
                    | [point; ptr; size; name] ->
                    (
                        let ptr = Stubs.GdbArgs.parse ptr in
                        let size = Stubs.GdbArgs.parse size in
                        buf point ptr size name
                    )
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let ret_reg_name = "ret_reg"

                let ret_reg_spec = []

                let ret_reg_desc = "return value register"

                let ret_reg func _ _ =
                    let sto = Function.get_iret 0 func in
                    let name = Printf.sprintf "%s_ret" func.Function.fname in
                    let kind = Sink.Auto((FuncStub(ret_reg_name)), ret_reg_name) in
                    SinkStubsBase.OnRet("ret_reg sink", lazy (Sink.create ~kind name (Expr.Var(sto))))

                let ret_reg_parsr = function
                    | [] -> ret_reg
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let arg_reg_name = "arg_reg"

                let arg_reg_spec = ["argnum"]

                let arg_reg_desc = "argument register number <argnum>"

                let arg_reg argnum func _ _ =
                    let sto = Function.get_iarg argnum func in
                    let name = Printf.sprintf "%s_arg_%d" func.Function.fname argnum in
                    let kind = Sink.Auto((FuncStub(arg_reg_name)), arg_reg_name) in
                    SinkStubsBase.Sink(Sink.create ~kind name (Expr.Var(sto)))

                let arg_reg_parsr = function
                    | [argnum] ->
                    (
                        try
                            let argnum = int_of_string argnum in
                            arg_reg argnum
                        with _ -> raise (Invalid_argument("invalid function argument number"))
                    )
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let _ =
                    register buf_name buf_spec buf_desc SinkStubsBase.SnkKind buf_parsr;
                    register ret_reg_name ret_reg_spec ret_reg_desc SinkStubsBase.SnkKind ret_reg_parsr;
                    register arg_reg_name arg_reg_spec arg_reg_desc SinkStubsBase.SnkKind arg_reg_parsr
            end
    end

let init_analysis ?restrict ?stubs ?(tag = None) name =
    let restrict = 
        match restrict with
        | Some(restrict) -> restrict
        | None -> parse_restrict @@ restrict_opt#get ?tag
    in
    let stubs = 
        match stubs with
        | Some(stubs) -> stubs
        | None -> SinkStubs.create @@ SinkStubs.cli_specs#get ?tag 
    in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = A.set_state a @@ S.set_restrict restrict @@ A.get_state a in
    let todo a =
        a
            |> A.FunctionCallbacks.add "auto snk" (SinkStubs.run stubs)
    in
    A.FunctionCallbacks.add "auto snk start" (A.FunctionCallbacks.start_callback ~name:"auto snk start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        type p = A.t

        module A = A

        let name = "autosink"
        let desc = "Generate sinks automatically based on specified stubs."

        let init ?(tag = None) () =
            init_analysis ~tag "Auto-Sink"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option PB.name "-restrict" (Options.TaggedOptions.String(restrict_opt)) "Restrict a sink to a smaller byte interval (format: snk_name:lo-hi;...).";
    SinkStubs.register_options "autosink"
