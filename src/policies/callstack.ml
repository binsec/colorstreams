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

module S =
    struct
        type t =
            {
                n : int;
                prev : int;
                curr : string
            }

        let create ?(tag = None) () =
            {n = 0; prev = 0; curr = ""}

        let add_stats_to_bundle b _ = b
    end

module A = Analysis.Make(Analysis.NoneBank) (S)

let check instr a =
    try
        let state = A.get_state a in
        if instr.Instruction.raw_ins = "ret " && state.n > 0 
        then 
        (
            let prev = state.n in
            let n = state.n - 1 in
            A.set_state a {state with prev; n}
        )
        else
        (
            let callex = Str.regexp {|call*|} in
            if Str.string_match callex instr.Instruction.raw_ins 0
            then 
            (
                let prev = state.n in
                let n = state.n + 1 in
                A.set_state a {state with prev; n}
            )
            else
            (
                if instr.Instruction.fname <> state.curr
                then
                (
                    let arr = if state.prev > state.n then " < " else " > " in
                    let msg = lazy ((Printf.sprintf "[%d]" state.n) ^ (String.make state.n '-') ^ arr ^ instr.Instruction.fname) in
                    let curr = instr.Instruction.fname in
                    Message.Wrap.send (Message.Wrap.Info(msg));
                    A.set_state a {state with curr}
                )
                else a
            )
        )
    with e -> 
    (
        Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printexc.to_string e)));
        a
    )

let init_analysis 
    ?(register_stats = true)
    ?(tag = None)
        name =
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    if register_stats
    then Stats.register ~silent:true @@ A.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ A.get_name a) a
    ;
    let todo a =
        A.InstructionCallbacks.add ~priority:100 "check" check a
    in
    A.FunctionCallbacks.add "callstack start" (A.FunctionCallbacks.start_callback ~name:"callstack start" ~add_sink_checks:false ~add_source_checks:false ~todo) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "callstack"
        let desc = "Callstack trace."

        let init ?(tag = None) () =
            init_analysis ~tag "Callstack"
    end

module P = Policy.Make (PB)

let () =
    Policy.register_policy (module P)
