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

let data_lim_opt = new Options.TaggedOptions.opt (-1)
let split_data_opt = new Options.TaggedOptions.opt false

class detection_catcher =
    object (self)
        inherit Message.handler () as super

        val mutable detections = []

        method handle msg =
            match msg.Message.ext_value with
            | Result.Result(Result.Custom(Oob_check.Res.CustomResult(detec), _)) -> 
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

type Sink.ext_kind +=
    | OOBBase of Oob_check.Res.result
    | OOBSize of Oob_check.Res.result
    | OOBWData of Oob_check.Res.result
    | OOBWDataNext of Oob_check.Res.result

module S =
    struct
        type t =
            {
                checker : Oob_check.t;
                mh : detection_catcher;
                data_lim : int option;
                split_data : bool
            }

        let create ?(tag = None) () =
            let checker = Oob_check.init_analysis ~tag "OOB Checker" in
            let mh = new detection_catcher in
            let data_lim = None in
            let split_data = false in
            {checker; mh; data_lim; split_data}

        let add_stats_to_bundle b _ = b

        let set_data_lim l s =
            if l <= 0
            then {s with data_lim = None}
            else {s with data_lim = Some(l)}

        let set_split_data split_data s =
            {s with split_data}
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

type t = A.t

let callback line a =
    let s = A.get_state a in
    let checker = Message.with_handler (fun () -> Oob_check.A.analyze line s.checker) (s.mh :> Message.handler) in
    let a, s = List.fold_left
        (
            fun (a, s) e ->
            (
                let name = Printf.sprintf "Detection_%d_OOB_%s" e.Result.result.Oob_check.ResVal.id @@ Oob_check.ResVal.pp_rw e.Result.result.Oob_check.ResVal.rw in
                let check_sink suff kind expr s =
                    let name = name ^ suff in
                    let snk = Sink.create ~kind name expr in
                    Message.Wrap.send (Analysis.Wrap.Sink(snk));
                    s
                in
                let check_data_maybe_next s =
                    let mk_expr expr =
                        let suff = "_wdata" in
                        let desc = "OOB written data" in
                        let mk_byte_split expr =
                            List.init (Expr.size expr)
                                (
                                    fun byte ->
                                        let suff = Printf.sprintf "%s_byte_%d" suff byte in
                                        let desc = Printf.sprintf "%s byte %d" desc byte in
                                        let expr = Expr.Unop((Expr.Restrict(byte, byte)), expr) in
                                        suff, desc, expr
                                )
                        in
                        let expr =
                            match s.S.data_lim with
                            | Some(lim) ->
                            (
                                if lim < Expr.size expr
                                then Expr.Unop((Expr.Restrict(0, lim - 1)), expr)
                                else expr
                            )
                            | _ -> expr
                        in
                        if s.S.split_data
                        then mk_byte_split expr
                        else [suff, desc, expr]
                    in
                    match e.Result.result.Oob_check.ResVal.rw with
                    | Oob_check.ResVal.Write(AfterUpdate) ->
                    (
                        let cbname = name ^ "_after_update" in
                        let snks = List.map 
                            (
                                fun (suff, desc, expr) -> 
                                    let snk = Sink.create ~kind:(Sink.Auto((OOBWDataNext(e)), desc)) (name ^ suff) expr in
                                    snk
                            ) 
                            @@ mk_expr (Expr.Var(e.result.target))
                        in
                        let cb line a =
                            match line with
                            | Trace.Ins(_) -> 
                            (
                                let a = List.fold_left
                                    (
                                        fun a snk ->
                                            Message.Wrap.send (Analysis.Wrap.Sink(snk));
                                            a
                                    )
                                    a snks
                                in
                                A.TraceCallbacks.remove cbname a
                            )
                            | _ -> a
                        in
                        (A.TraceCallbacks.add ~priority:50 cbname cb a), s
                    )
                    | Oob_check.ResVal.Write(Oob_check.ResVal.Expr(expr))
                    | Oob_check.ResVal.Write(Oob_check.ResVal.Pattern(expr)) -> a, List.fold_left 
                        (
                            fun s (suff, desc, expr)-> 
                                check_sink suff (Sink.Auto((OOBWData(e)), desc)) expr s
                        )
                        s 
                        @@ mk_expr expr
                    | _ -> a, s
                in
                s
                    |> check_sink "_base" (Sink.Auto((OOBBase(e)), "OOB base")) e.result.base
                    |> check_sink "_size" (Sink.Auto((OOBSize(e)), "OOB size")) e.result.size
                    |> check_data_maybe_next
            )
        )
        (a, {s with checker}) s.mh#get
    in
    A.set_state a s

let end_callback signal a =
    let s = A.get_state a in
    A.set_state a {s with S.checker = Oob_check.A.fini signal s.S.checker}

let init_analysis ?data_lim ?split_data ?(tag = None) name =
    let data_lim = data_lim_opt#maybe_get ?tag data_lim in
    let split_data = split_data_opt#maybe_get ?tag split_data in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = A.set_state a @@ S.set_split_data split_data @@ S.set_data_lim data_lim @@ A.get_state a in
    let todo a =
        a
            |> A.TraceCallbacks.add "analysis" callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "oob auto sink start" (A.FunctionCallbacks.start_callback ~name:"oob auto sink start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "oobautosink"
        let desc = "Generate sinks automatically based on OOB Checker detections."

        let init ?(tag = None) () =
            init_analysis ~tag "OOB Auto-Sink"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option "oobautosink" "-data-lim" (Options.TaggedOptions.Int(data_lim_opt)) "Limit data sink byte size (default: no limit).";
    Options.register_policy_alias "oobautosink" "-data-lim" "-dl";
    Options.register_policy_option "oobautosink" "-split-data" (Options.TaggedOptions.Bool(split_data_opt)) "Split data sinks into individual bytes if larger than 8."
