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

let sdetec_opt = new Options.TaggedOptions.opt ""
let reporter_opt = new Options.TaggedOptions.opt "default"

module ResVal =
    struct
        type param = Oob_capabilities.ResVal.param

        module Triple = Oob_capabilities.ResVal.Triple

        type score = Triple.t

        type report = ..

        type t =
            {
                detection : Cfhijack_check.Res.result;
                maps : MemMaps.t;
                ptr : param;
                report : (report * Result.generic Lazy.t) option
            }

        module Reporters =
            struct
                let registered = ref Utils.StringMap.empty

                let register name mk_report =
                    if Utils.StringMap.mem name !registered
                    then raise (Failure(Printf.sprintf "could not register cfh capability reporter <%s>: name already in use" name))
                    else registered := Utils.StringMap.add name mk_report !registered

                let pp () =
                    let msg = Utils.StringMap.fold (fun name _ res -> res ^ "\n- " ^ name) !registered "Available CFH capability reporters:" in
                    Message.Wrap.send (Message.Wrap.BigInfo(msg))

                let _ =
                    Options.register_policy_option "cfhcapa" "-list-reporters" (Options.TaggedOptions.Untagged(fun () -> Options.register_todo pp)) "List available CFH capability reporters."
            end

        let create detection ptr =
            {detection; maps = MemMaps.get (); ptr; report = None}

        let create_report name r =
            try
                let mk_report = Utils.StringMap.find name !Reporters.registered in
                {r with report = mk_report r}
            with Not_found -> raise (Failure(Printf.sprintf "unknown cfh capability reporter <%s>" name))

        let to_generic r =
            let ptrres =
                let snkres = Result.mk_sto_expr ~label:"Sink" r.ptr.snk.Sink.expr in
                Result.mk_res_list ~label:"ptr" [snkres; Symbolic.Properties.Sns.Res.export r.ptr.domain]
            in
            let reportres =
                match r.report with
                | Some(_, lazy report) -> Result.create ~label:"report" report
                | _ -> Result.mk_string ~label:"report" "N/A"
            in
            Result.Res([Cfhijack_check.Res.export r.detection; ptrres; reportres])

        module DefaultReporter =
            struct
                type entry =
                    {
                        map : MemMaps.entry;
                        ptrdomain : Symbolic.Properties.Sns.ResVal.t;
                        score : score;
                    }
        
                type report += Default of entry list * score
        
                let to_generic_default_report rep =
                    match rep with mmaps, score ->
                    (
                        let mk_report_entry e =
                            let mapres = Result.create ~label:"mapping" @@ MemMaps.entry_to_generic e.map in
                            let ptrdomainres = Symbolic.Properties.Sns.Res.export @@ Symbolic.Properties.Sns.Res.create ~label:"ptr domain" e.ptrdomain in
                            let scoreres = Result.create ~label:"overall score" @@ Triple.to_generic e.score in
                            Result.mk_res_list ~label:(MemMaps.pp_entry_desc e.map) [mapres; ptrdomainres; scoreres]
                        in
                        let mapres = Result.mk_res_list ~label:"memory mapping breakdown" @@ List.map mk_report_entry mmaps in
                        let scoreres = Result.create ~label:"overall score" @@ Triple.to_generic score in
                        Result.Res([scoreres; mapres])
                    )
        
                let create_default_report r =
                    let ptrdomain = r.ptr.domain.Result.result in
                    let create_entry acc tot map =
                        let valid = MemMaps.check_perms ~x:(Some(true)) map in
                        let ptrdomain, _ = Symbolic.Properties.Sns.ResVal.cut map.MemMaps.range ptrdomain in
                        let maxmax = Interval.card map.MemMaps.range in
                        if MemMaps.is_kernel_space map || not valid 
                        then acc, tot, None
                        else if ptrdomain.Symbolic.Properties.Sns.ResVal.itvs = []
                        then acc, Z.add tot maxmax, None
                        else
                        (
                            let min, maybe_min, max = List.fold_left
                                (
                                    fun (min, maybe_min, max) (itv, card) ->
                                        let card = Z.sub (Lazy.force card).Interval.hi Z.one in
                                        match itv with
                                        | Symbolic.Properties.Sns.Strong(_) -> Z.add min card, Z.add maybe_min card, Z.add max card
                                        | MaybeStrong(_) -> Z.add min @@ Z.of_int 2, Z.add maybe_min card, Z.add max card
                                        | NotStrong(_) -> Z.add min @@ Z.of_int 2, Z.add maybe_min @@ Z.of_int 2, Z.add max card
                                        | Unknown(_) -> min, maybe_min, Z.add max card
                                )
                                (Z.zero, Z.zero, Z.zero) ptrdomain.Symbolic.Properties.Sns.ResVal.itvs
                            in
                            let score = Triple.apply_float (/.) {min = Z.to_float min; maybe_min = Z.to_float maybe_min; max = Z.to_float max} @@ Z.to_float maxmax in
                            Triple.apply_triple (+.) acc @@ Triple.apply_float ( *.) score @@ Z.to_float maxmax, 
                            Z.add tot maxmax,
                            Some({map; ptrdomain; score})
                        )
                    in
                    let entries, sum, tot = List.fold_right
                        (
                            fun (_, map) (entries, sum, tot) ->
                                let sum, tot, e = create_entry sum tot map in
                                match e with
                                | Some(e) -> e::entries, sum, tot
                                | _ -> entries, sum, tot
                        )
                        r.maps ([], Triple.zero, Z.zero)
                    in
                    (*let entries = List.filter_map (fun (_, map) -> create_entry map) r.maps in
                    let overall_score = Triple.apply_float (/.) (List.fold_left (fun acc e -> Triple.apply_triple (+.) acc e.score) {min = Float.zero; maybe_min = Float.zero; max = Float.zero} entries) @@ Float.of_int @@ List.length entries in*)
                    let overall_score = Triple.apply_float (/.) sum @@ Z.to_float tot in
                    Some(Default(entries, overall_score), lazy (to_generic_default_report (entries, overall_score)))
        
                let _ =
                    Reporters.register "default" create_default_report
            end
    end

module Res = Result.Make (ResVal)

module S =
    struct
        type t =
            {
                symb : Symbolic_policy.SA.t;
                reporter : string;
                selected_detections : int list
            }

        let create ?(tag = None) () =
            let symb = Symbolic_policy.init_analysis ~tag "Symbolic" in
            {symb; reporter = "default"; selected_detections = []}

        let select_detections selected_detections s =
            {s with selected_detections}

        let set_reporter reporter s =
            {s with reporter}

        let check_selected detec s =
            if s.selected_detections = []
            then true
            else
            (
                let id = detec.Result.result.Cfhijack_check.ResVal.id in
                try
                    ignore @@ List.find (fun i -> i = id) s.selected_detections;
                    true
                with Not_found -> false
            )

        let add_stats_to_bundle b _ = b
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

type t = A.t

let analyze trace a =
    match trace with
    | Trace.Snk({Sink.kind = Sink.Auto(Cfhijack_auto_sink.CFHijack(detec), _); _} as snk) ->
    (
        let state = A.get_state a in
        if S.check_selected detec state
        then
        (
            let domain =
                match Symbolic_policy.SB.get_sink_formula snk @@ Symbolic_policy.SA.get_bank state.symb with
                | Some(proj, sf) -> Symbolic.Properties.Sns.sns proj sf
                | None -> assert false
            in
            let ptr = {Oob_capabilities.ResVal.snk; domain} in
            let res = ResVal.create_report state.reporter @@ ResVal.create detec ptr in
            let label = Printf.sprintf "Detection %d" detec.Result.result.Cfhijack_check.ResVal.id in
            Res.send ~prefix:"RESULT" @@ Res.create ~label res
        )
    )
    | _ -> ()

let callback trace a =
    let state = A.get_state a in
    A.set_state a {state with symb = Symbolic_policy.SA.analyze trace state.symb}

let end_callback signal a =
    let s = A.get_state a in
    A.set_state a {s with symb = Symbolic_policy.SA.fini signal s.symb}

let init_analysis ?(tag = None) ?selected_detections ?reporter name =
    let selected_detections = 
        match selected_detections with
        | Some(selected_detections) -> selected_detections
        | None -> 
        (
            let l = String.split_on_char ';' @@ sdetec_opt#get ?tag in
            if l = [""]
            then []
            else List.map int_of_string l
        )
    in
    let reporter = reporter_opt#maybe_get ?tag reporter in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = A.set_state a 
        @@ S.set_reporter reporter
        @@ S.select_detections selected_detections 
        @@ A.get_state a 
    in
    let todo a =
        a
            |> A.TraceCallbacks.add_const "analysis" analyze
            |> A.TraceCallbacks.add ~priority:100 "update" callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "cfh capa start" (A.FunctionCallbacks.start_callback ~name:"cfh capa start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "cfhcapa"
        let desc = "Analyze Control-Flow Hijacking capabilities (note: currently only supports absolute jumps)."

        let init ?(tag = None) () =
            init_analysis ~tag "CFH Capabilities"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option "cfhcapa" "-select-detections" (Options.TaggedOptions.String(sdetec_opt)) "Only analyze specified detections (format: 1;2;10;...).";
    Options.register_policy_alias "cfhcapa" "-select-detections" "-sd";
    Options.register_policy_option "oobcapa" "-reporter" (Options.TaggedOptions.String(reporter_opt)) "Select an OOB capability report style."
