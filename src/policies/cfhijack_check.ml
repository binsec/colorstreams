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

module ResVal =
    struct
        type kind = Call | Jump | Ret

        type t =
            {
                id : int;
                loc : Trace.Gdb.LocRes.result option;
                target : Storage.t;
                kind : kind;
                taint : Bytedep_policy.MTT.t option list
            }

        let cnt = ref 0

        let create kind target taint =
            let id = !cnt in
            cnt := !cnt + 1;
            let loc = Trace.Gdb.get_location @@ Trace.get_gdb () in
            {id; loc; target; kind; taint}

        let pp_kind = function
            | Call -> "Call"
            | Jump -> "Jump"
            | Ret -> "Ret"

        let to_generic r =
            let locres =
                match r.loc with
                | Some(loc) -> Trace.Gdb.LocRes.export loc
                | _ -> Result.mk_string ~label:"location" "not found"
            in
            let targetres = Result.mk_storage ~label:"target" r.target in
            let kindres = Result.mk_string ~label:"kind" @@ pp_kind r.kind in
            let ttres = Result.mk_list ~label:"taint" @@ List.map Bytedep_policy.MTT.to_generic r.taint in
            Result.Res([targetres; kindres; ttres; locres])
    end

module Res =
    struct
        include Result.Make(ResVal)

        let create_detection r =
            let label = Printf.sprintf "Detection %d" r.ResVal.id in
            create ~label r
    end

module S =
    struct
        type t =
            {
                prevtaint : Bytedep_policy.A.t;
                taint : Bytedep_policy.A.t;
            }

        let create ?(tag = None) () =
            let taint = Bytedep_policy.init_analysis ~tag "CFH Taint" 
                |> Bytedep_policy.A.set_ignore_sinks true
            in
            {prevtaint = taint; taint}

        let add_stats_to_bundle stats _ = stats

        let update_taint trace s =
            {prevtaint = s.taint; taint = Bytedep_policy.A.analyze trace s.taint}

        let end_taint signal s =
            let taint = Bytedep_policy.A.fini signal s.taint in
            {prevtaint = taint; taint}
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

let update_callback trace a =
    A.set_state a @@ S.update_taint trace @@ A.get_state a

let expr = Str.regexp {|call\|ret\|jmp|}

let do_check kind a sto =
    Message.Wrap.send (Message.Wrap.Debug("CHECK", lazy (Storage.pp sto)));
    match Bytedep_policy.TB.get_taint sto @@ Bytedep_policy.A.get_bank @@ (A.get_state a).prevtaint with
    | Some(taint) -> 
    (
        let detec = Res.create_detection @@ ResVal.create kind sto taint in
        Res.send ~prefix:"RESULT" detec
    )
    | _ -> ()

let check_callback instr a =
    if Str.string_match expr instr.Instruction.raw_ins 0
    then
    (
        let kind =
            match Str.matched_string instr.raw_ins with
            | "call" -> ResVal.Call
            | "ret" -> ResVal.Ret
            | "jmp" -> ResVal.Jump
            | _ -> assert false
        in
        List.iter (do_check kind a) instr.Instruction.direct_reads
    )

let end_callback signal a =
    A.set_state a @@ S.end_taint signal @@ A.get_state a

let init_analysis
    ?(register_stats = true)
    ?(tag = None)
        name =
    let a = A.create ~tag name in
    if register_stats
    then Stats.register ~silent:true @@ A.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ A.get_name a) a
    ;
    let todo a =
        a
            |> A.TraceCallbacks.add ~priority:100 "update" update_callback
            |> A.InstructionCallbacks.add_const ~priority:100 "check" check_callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "cfhijack check start" (A.FunctionCallbacks.start_callback ~name:"cfhijack check start" ~todo ~add_sink_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "cfhcheck"
        let desc = "Control-Flow Hijacking checker based on taint."

        let init ?(tag = None) () =
            init_analysis ~tag "CFH Checker"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P)
