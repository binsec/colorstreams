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

let byte_split_opt = new Options.TaggedOptions.opt false

class detection_catcher =
    object(self)
        inherit Message.handler () as super

        val mutable detections = []

        method handle msg =
            match msg.Message.ext_value with
            | Result.Result(Result.Custom(Cfhijack_check.Res.CustomResult(detec), _)) ->
            (
                detections <- detec::detections;
                super#handle msg
            )
            | _ -> super#handle msg

        method get =
            let res = detections in
            detections <- [];
            res
    end

type Sink.ext_kind += CFHijack of Cfhijack_check.Res.result

module S =
    struct
        type t =
            {
                checker : Cfhijack_check.A.t;
                mh : detection_catcher;
                byte_split : bool
            }

        let create ?(tag = None) () =
            let checker = Cfhijack_check.init_analysis ~tag "CFH Checker" in
            let mh = new detection_catcher in
            let byte_split = false in
            {checker; mh; byte_split}

        let set_byte_split byte_split s =
            {s with byte_split}

        let add_stats_to_bundle b _ = b
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

let callback trace a =
    let s = A.get_state a in
    let checker = Message.with_handler (fun () -> Cfhijack_check.A.analyze trace s.checker) (s.mh :> Message.handler) in
    List.iter
        (
            fun detec ->
                let name = Printf.sprintf "Detection_%d_CFH_%s" detec.Result.result.Cfhijack_check.ResVal.id @@ Cfhijack_check.ResVal.pp_kind detec.result.kind in
                let expr = Expr.Var(detec.result.target) in
                let snks =
                    if s.byte_split
                    then List.init (Expr.size expr)
                        (
                            fun byte ->
                                let name = Printf.sprintf "%s_byte_%d" name byte in
                                let desc = Printf.sprintf "CFH byte %d" byte in
                                let kind = Sink.Auto(CFHijack(detec), desc) in
                                let expr = Expr.Unop(Expr.Restrict(byte, byte), expr) in
                                Sink.create ~kind name expr
                        )
                    else
                    (
                        let kind = Sink.Auto(CFHijack(detec), "CFH") in
                        [Sink.create ~kind name expr]
                    )
                in
                List.iter (fun snk -> Message.Wrap.send (Analysis.Wrap.Sink(snk))) snks
        )
        s.mh#get
    ;
    A.set_state a {s with checker}

let end_callback signal a =
    let s = A.get_state a in
    A.set_state a {s with S.checker = Cfhijack_check.A.fini signal s.S.checker}

let init_analysis ?byte_split ?(tag = None) name =
    let byte_split = byte_split_opt#maybe_get ?tag byte_split in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = A.set_state a @@ S.set_byte_split byte_split @@ A.get_state a in
    let todo a =
        a
            |> A.TraceCallbacks.add "analysis" callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "cfh auto sink start" (A.FunctionCallbacks.start_callback ~name:"cfh auto sink start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "cfhautosink"

        let desc = "Generate sinks automatically based on CFH Checker detections."

        let init ?(tag = None) () =
            init_analysis ~tag "CFH Auto-Sink"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option P.name "-byte-split" (Options.TaggedOptions.Bool(byte_split_opt)) "Split sinks into individual bytes."
