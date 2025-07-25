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

type Sink.ext_kind += Ret

let callback instr a =
    let retex = Str.regexp {|ret|} in
    if Str.string_match retex instr.Instruction.raw_ins 0
    then
    (
        Message.Wrap.send (Message.Wrap.Debug("CHECK-RET", lazy ("checking ret in " ^ instr.Instruction.fname)));
        let fname = instr.Instruction.fname in
        let name = Printf.sprintf "%s_ret" fname in
        let rsp = Storage.Register("rsp", 8) in
        let addr = Trace.stoval rsp in
        let ret = Storage.Memory(addr, 8) in
        let snk_rsp = Sink.create ~fname ~kind:(Sink.Auto(Ret, "return")) (name ^ "_rsp") (Expr.Var(rsp)) in
        let snk_ret = Sink.create ~fname ~kind:(Sink.Auto(Ret, "return")) name (Expr.Var(ret)) in
        Message.Wrap.send (Analysis.Wrap.Sink(snk_rsp));
        Message.Wrap.send (Analysis.Wrap.Sink(snk_ret))
    )

let init_analysis ?(tag = None) name =
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let todo a =
        a
            |> A.InstructionCallbacks.add_const "cb" callback
    in
    A.FunctionCallbacks.add "start" (A.FunctionCallbacks.start_callback ~name:"start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "retautosink"
        let desc = "Generate sinks automatically for return addresses on return instructions."

        let init ?(tag = None) () =
            init_analysis ~tag "Ret Auto-Sink"

     end
    
module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P)
