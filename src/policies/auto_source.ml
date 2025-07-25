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

module A = Analysis.Make (Analysis.NoneBank) (Analysis.NoneState)

module SourceStubsBase =
    struct
        let modulename = "source"

        type state = A.t
        type action = Source of Source.t | OnRet of string * Source.t Lazy.t | NextInstr of string * Source.t Lazy.t | Nothing
        type kind = SrcKind

        let pp_kind _ =
            "source"

        let check_kind _ _ a _ =
            a, true

        let send_source src a =
            Message.Wrap.send (Analysis.Wrap.Source(src));
            A.analyze (Trace.Src(src)) a

        let act _ call_id a = function
            | Source(src) -> send_source src a
            | OnRet(name, src) -> A.FunctionCallbacks.on_callret call_id name (fun _ a -> send_source (Lazy.force src) a) a
            | NextInstr(name, src) -> A.InstructionCallbacks.add ~priority:999 name 
                (
                    fun _ a ->
                        let a = send_source (Lazy.force src) a in
                        A.InstructionCallbacks.remove name a
                )
                a
            | _ -> a
    end

module SourceStubs =
    struct
        include Stubs.Make (SourceStubsBase)

        module DefaultStubs =
            struct
                type Source.ext_kind += FuncStub of string

                let bytes_to_buf_name = "bytes_to_buf"

                let bytes_to_buf_spec = ["entry / ret"; "buf"; "size"; "source name"; "source desc"]

                let bytes_to_buf_desc = "<size> first bytes of <buf> after entry or before return (format: " ^ Stubs.GdbArgs.spec ^ ")"

                let bytes_to_buf point buf size name desc func _ _ =
                    let bufexpr = Stubs.GdbArgs.get func buf in
                    let sizeexpr = Stubs.GdbArgs.get func size in
                    let sto = lazy (Storage.Memory(Lazy.force bufexpr, Z.to_int @@ Lazy.force sizeexpr)) in
                    let kind = Source.Auto((FuncStub(bytes_to_buf_name)), bytes_to_buf_name) in
                    let src = lazy (Source.create ~name ~kind (Lazy.force sto) desc) in
                    if point = "entry"
                    then SourceStubsBase.Source(Lazy.force src)
                    else if point = "ret"
                    then SourceStubsBase.OnRet("bytes_to_buf source", src)
                    else raise (Invalid_argument(point))

                let bytes_to_buf_parsr = function
                    | [point; buf; size; name; desc] ->
                    (
                        let buf = Stubs.GdbArgs.parse buf in
                        let size = Stubs.GdbArgs.parse size in
                        bytes_to_buf point buf size name @@ String.split_on_char '/' desc
                    )
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let cmd_arg_name = "cmd_arg"

                let cmd_arg_spec = ["argnum"; "source name"; "source desc"]

                let cmd_arg_desc = "command line string number <argnum>"

                let cmd_arg_cnt = ref 0

                let cmd_arg argnum name desc func callid s =
                    let buf = Stubs.GdbArgs.parse ("gdb<((char**)@1)[" ^ argnum ^ "]>") in
                    let len = Stubs.GdbArgs.parse ("strlen<gdb<((char**)@1)[" ^ argnum ^ "]>>") in
                    let cnt = !cmd_arg_cnt in
                    cmd_arg_cnt := cnt + 1;
                    match bytes_to_buf "entry" buf len name desc func callid s with
                    | SourceStubsBase.Source(src) -> SourceStubsBase.NextInstr(Printf.sprintf "cmd_arg_src_%d" cnt, lazy src)
                    | _ -> assert false

                let cmd_arg_parsr = function
                    | [argnum; name; desc] -> cmd_arg argnum name @@ String.split_on_char '/' desc
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let ret_reg_name = "ret_reg"

                let ret_reg_spec = ["source desc"]

                let ret_reg_desc = "return value register"

                let ret_reg desc func _ _ =
                    let sto = Function.get_iret 0 func in
                    let name = Printf.sprintf "%s_ret" func.Function.fname in
                    let kind = Source.Auto((FuncStub(ret_reg_name)), ret_reg_name) in
                    SourceStubsBase.OnRet("ret_reg source", lazy (Source.create ~name ~kind sto desc))

                let ret_reg_parsr = function
                    | [desc] -> ret_reg @@ String.split_on_char '/' desc
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let arg_reg_name = "arg_reg"

                let arg_reg_spec = ["argnum"; "desc"]

                let arg_reg_desc = "argument register number <argnum>"

                let arg_reg argnum desc func _ _ =
                    let sto = Function.get_iarg argnum func in
                    let name = Printf.sprintf "%s_arg_%d" func.Function.fname argnum in
                    let kind = Source.Auto((FuncStub(arg_reg_name)), arg_reg_name) in
                    SourceStubsBase.Source(Source.create ~name ~kind sto desc)

                let arg_reg_parsr = function
                    | [argnum; desc] ->
                    (
                        try
                            let argnum = int_of_string argnum in
                            arg_reg argnum @@ String.split_on_char '/' desc
                        with _ -> raise (Invalid_argument("invalid function argument number"))
                    )
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let _ =
                    register bytes_to_buf_name bytes_to_buf_spec bytes_to_buf_desc SourceStubsBase.SrcKind bytes_to_buf_parsr;
                    register cmd_arg_name cmd_arg_spec cmd_arg_desc SourceStubsBase.SrcKind cmd_arg_parsr;
                    register ret_reg_name ret_reg_spec ret_reg_desc SourceStubsBase.SrcKind ret_reg_parsr;
                    register arg_reg_name arg_reg_spec arg_reg_desc SourceStubsBase.SrcKind arg_reg_parsr
            end
    end

let init_analysis ?stubs ?(tag = None) name =
    let stubs = 
        match stubs with
        | Some(stubs) -> stubs
        | None -> SourceStubs.create @@ SourceStubs.cli_specs#get ?tag 
    in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let todo a =
        a
            |> A.FunctionCallbacks.add "auto src" (SourceStubs.run stubs)
    in
    A.FunctionCallbacks.add "auto src start" (A.FunctionCallbacks.start_callback ~name:"auto src start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        type p = A.t

        module A = A

        let name = "autosource"
        let desc = "Generate sources automatically based on specified stubs."

        let init ?(tag = None) () =
            init_analysis ~tag "Auto-Source"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    SourceStubs.register_options "autosource"
