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
let load_opt = new Options.TaggedOptions.opt ""

module ResVal =
    struct
        type param =
            {
                snk : Sink.t;
                domain : Symbolic.Properties.Sns.Res.result;
            }

        module Triple =
            struct
                type t =
                    {
                        min : float;
                        maybe_min : float;
                        max : float
                    }

                let zero = {min = 0.0; maybe_min = 0.0; max = 0.0}

                let apply_float func a f =
                    {min = func a.min f; maybe_min = func a.maybe_min f; max = func a.max f}

                let apply_triple func a b =
                    {min = func a.min b.min; maybe_min = func a.maybe_min b.maybe_min; max = func a.max b.max}

                let of_domain dom =
                    let min = Z.to_float @@ Lazy.force dom.Symbolic.Properties.Sns.ResVal.min in
                    let maybe_min = Z.to_float @@ Lazy.force dom.Symbolic.Properties.Sns.ResVal.maybe_min in
                    let max = Z.to_float @@ Lazy.force dom.Symbolic.Properties.Sns.ResVal.max in
                    {min; maybe_min; max}

                let to_generic t =
                    let minres = Result.mk_float ~label:"min" t.min in
                    let maybe_minres = Result.mk_float ~label:"maybe_min" t.maybe_min in
                    let maxres = Result.mk_float ~label:"max" t.max in
                    Result.Res([minres; maybe_minres; maxres])
            end

        type report = ..

        type t =
            {
                detection : Oob_check.Res.result;
                maps : MemMaps.t;
                base : param option;
                offset : param option;
                size : param option;
                data : param list;
                report : (report * Result.generic Lazy.t) option
            }

        module Reporters =
            struct
                let registered = ref Utils.StringMap.empty

                let register name desc (mk_report : (?similar:((string * t) list) -> t -> (report * Result.generic Lazy.t) option)) =
                    if Utils.StringMap.mem name !registered
                    then raise (Failure(Printf.sprintf "could not register oob capability reporter <%s>: name already in use" name))
                    else registered := Utils.StringMap.add name (desc, mk_report) !registered

                let pp () =
                    let msg = Utils.StringMap.fold (fun name (desc, _) res -> res ^ "\n- " ^ name ^ ":\n    " ^ desc) !registered "Available OOB capability reporters:" in
                    Message.Wrap.send (Message.Wrap.BigInfo(msg))

                let _ =
                    Options.register_policy_option "oobcapa" "-list-reporters" (Options.TaggedOptions.Untagged(fun () -> Options.register_todo pp)) "List available OOB capability reporters."
            end

        let create detection =
            {detection; maps = MemMaps.get (); base = None; offset = None; size = None; data = []; report = None}

        let create_report ?(similar = []) name r =
            try
                let _, mk_report = Utils.StringMap.find name !Reporters.registered in
                {r with report = mk_report ~similar r}
            with Not_found -> raise (Failure(Printf.sprintf "unknown oob capability reporter <%s>" name))

        let of_generic = function
            | Result.Res([detec; base; offset; size; data; _]) ->
            (
                let detection = Oob_check.Res.from_generic_result detec in
                let cnt = ref 0 in
                let param name = function
                    | [{Result.result = Result.StoExpr(expr); _}; domains] ->
                    (
                        let snk = {Sink.name = Identifier.create ~cnt name; expr; fname = ""; kind = Sink.Custom; more = ""; constr = None} in
                        let domain = Symbolic.Properties.Sns.domains_from_generic_results domains in
                        {snk; domain}
                    )
                    | _ -> assert false
                in
                let maybe_param name = function
                    | Result.Res(l) -> Some(param name l)
                    | Str("N/A") -> None
                    | _ -> assert false
                in
                let base = maybe_param "base" base.Result.result in
                let offset = maybe_param "offset" offset.Result.result in
                let size = maybe_param "size" size.Result.result in
                let data =
                    match data.Result.result with
                    | Res(_::params) ->
                    (
                        let aux = function
                            | {Result.result = Result.Res(l); _} -> param "data" l
                            | _ -> assert false
                        in
                        List.map aux params
                    )
                    | Str("input")
                    | Str("N/A") -> []
                    | _ -> assert false
                in
                {detection; maps = []; base; offset; size; data; report = None}
            )
            | _ -> assert false 

        let to_generic r =
            let mk_param p =
                let snkres = Result.mk_sto_expr ~label:"Sink" p.snk.Sink.expr in
                [snkres; Symbolic.Properties.Sns.Res.export p.domain]
            in
            let maybe f label = function
                | Some(p) -> Result.mk_res_list ~label @@ f p
                | None -> Result.mk_string ~label "N/A"
            in
            let datares =
                match r.detection.Result.result.Oob_check.ResVal.rw, r.data with
                | Oob_check.ResVal.Write(Oob_check.ResVal.Input), _ -> Result.mk_string ~label:"data" "input"
                | Write(wdata), _::_ -> Result.mk_res_list ~label:"data" ((Result.mk_string ~label:"kind" @@ Oob_check.ResVal.pp_wdata wdata)::(List.mapi (fun i p -> Result.mk_res_list ~label:(string_of_int i) @@ mk_param p) r.data))
                | _ -> Result.mk_string ~label:"data" "N/A"
            in
            let reportres =
                match r.report with
                | Some(_, lazy report) -> Result.create ~label:"report" report
                | _ -> Result.mk_string ~label:"report" "N/A"
            in
            Result.Res([Oob_check.Res.export r.detection; maybe mk_param "base" r.base; maybe mk_param "offset" r.offset; maybe mk_param "size" r.size; datares; reportres])

        module DefaultReporter =
            struct
                type score =
                    {
                        base : Triple.t;
                        size : Triple.t;
                        data : Triple.t option;
                        overall : Triple.t
                    }

                module MapKey =
                    struct
                        type t = MemMaps.entry

                        let compare a b =
                            Interval.compare a.MemMaps.range b.MemMaps.range
                    end

                module MapMap = Map.Make(MapKey)

                type default_report = 
                    {
                        alone : score MapMap.t;
                        similar : (int * string) list;
                        full : score MapMap.t option
                    }
        
                type report += Default of default_report
        
                let to_generic_default_report rep =
                    let mk_report_entry map score res =
                        let mk_score label s =
                            Result.create ~label @@ Triple.to_generic s
                        in
                        let mapres = Result.create ~label:"mapping" @@ MemMaps.entry_to_generic map in
                        let basescoreres = mk_score "base score" score.base in
                        let sizescoreres = mk_score "size score" score.size in
                        let datascoreres =
                            match score.data with
                            | Some(data) -> mk_score "data score" data
                            | _ -> Result.mk_string ~label:"data score" "N/A"
                        in
                        let scoreres = mk_score "overall score" score.overall in
                        let label = MemMaps.pp_entry_desc map in
                        (Result.mk_res_list ~label [mapres; basescoreres; sizescoreres; datascoreres; scoreres])::res
                    in
                    let alone = MapMap.fold mk_report_entry rep.alone [] in
                    match rep.full with
                    | Some(full) ->
                    (
                        let aloneres = Result.mk_res_list ~label:"alone" alone in
                        let by_file = List.fold_left
                            (
                                fun res (id, file) ->
                                    try
                                        let ids = Utils.StringMap.find file res in
                                        Utils.StringMap.add file (id::ids) res
                                    with Not_found -> Utils.StringMap.add file [id] res
                            )
                            Utils.StringMap.empty rep.similar
                        in
                        let simres = Result.mk_res_list ~label:"history" 
                            @@ Utils.StringMap.fold (fun file ids res -> (Result.mk_list ~label:file @@ List.map (fun id -> Result.Int(Z.of_int id)) ids)::res) by_file []
                        in
                        let fullres = Result.mk_res_list ~label:"full"
                            @@ MapMap.fold mk_report_entry full []
                        in
                        Result.Res([aloneres; simres; fullres])
                    )
                    | None -> Result.Res(alone)

                let create_default_report ?(similar = []) r =
                    let obji =
                        match r.detection.Result.result.Oob_check.ResVal.reason with
                        | Oob_check.ResVal.TagOOB(tt) 
                        | TagUAF(tt) -> Some(Interval.create ~lo:tt.Oob_check.TT.base ~hi:(Address.add_int tt.base tt.size))
                        | _ -> None
                    in
                    let datascore domains =
                        assert (not (domains = []));
                        let acc (t, div) domain =
                            let size = domain.Result.result.Symbolic.Properties.Sns.ResVal.size in
                            let maxmax = Z.to_float @@ Z.shift_left Z.one size in
                            let counts = Triple.of_domain domain.Result.result in
                            let t = Triple.apply_triple (+.) t @@ Triple.apply_float (/.) counts maxmax in
                            t, div +. 1.0
                        in
                        let t, div = List.fold_left acc (Triple.zero, 0.0) domains in
                        Triple.apply_float (/.) t div
                    in
                    let rangescore map basedomain sizedomain =
                        let basedomain = basedomain.Result.result in
                        let sizedomain = sizedomain.Result.result in
                        let valid =
                            match r.detection.Result.result.Oob_check.ResVal.rw with
                            | Oob_check.ResVal.Read -> MemMaps.check_perms ~r:(Some(true)) map
                            | Write(_) -> MemMaps.check_perms ~w:(Some(true)) map
                        in
                        let basedomain, _ = Symbolic.Properties.Sns.ResVal.cut map.MemMaps.range basedomain in
                        let sizemaxitv = Interval.create ~lo:Z.one ~hi:(Z.add Z.one @@ Interval.card map.range) in
                        let sizedomain, _ = Symbolic.Properties.Sns.ResVal.cut sizemaxitv sizedomain in
                        if MemMaps.is_kernel_space map || not valid || basedomain.Symbolic.Properties.Sns.ResVal.itvs = [] || sizedomain.itvs = []
                        then None
                        else
                        (
                            let do_score itv_score maxitv dom =
                                let maybe_account_for_constr itv realcard score =
                                    if realcard = Interval.card itv
                                    then score
                                    else score *. (Z.to_float realcard) /. (Z.to_float @@ Interval.card itv)
                                in
                                let min, maybe_min, max = List.fold_left 
                                    (
                                        fun (min, maybe_min, max) (itv, card) ->
                                            let card = Lazy.force card in
                                            let maxscore =
                                                let itv = Symbolic.Properties.Sns.get_itv itv in
                                                maybe_account_for_constr itv (Z.sub card.Interval.hi Z.one) @@ itv_score itv    
                                            in
                                            let minscore = 
                                                let itv = Symbolic.Properties.Sns.get_itv itv in
                                                if Interval.card itv = Z.one
                                                then itv_score itv 
                                                else (itv_score @@ Interval.create ~lo:itv.lo ~hi:(Z.add itv.lo Z.one)) +. (itv_score @@ Interval.create ~lo:(Z.sub itv.hi Z.one) ~hi:itv.hi)
                                            in
                                            match itv with
                                            | Symbolic.Properties.Sns.Strong(_) -> (min +. maxscore), (maybe_min +. maxscore), (max +. maxscore)
                                            | MaybeStrong(_) -> (min +. minscore), (maybe_min +. maxscore), (max +. maxscore)
                                            | NotStrong(_) -> (min +. minscore), (maybe_min +. minscore), (max +. maxscore)
                                            | Unknown(_) -> min, maybe_min, (max +. maxscore)
                                    )
                                    (Float.zero, Float.zero, Float.zero) dom.Symbolic.Properties.Sns.ResVal.itvs
                                in
                                let maxmax = itv_score maxitv in
                                Triple.apply_float (/.) {min; maybe_min; max} maxmax
                            in
                            let rec itv_score_proximity pivot itv =
                                if Interval.contains itv pivot
                                then List.fold_left (fun res itv -> res +. itv_score_proximity pivot itv) Float.one @@ Interval.split pivot itv
                                else
                                (
                                    let log z =
                                        let log z =
                                            if z = Z.zero
                                            then (-. Float.one)
                                            else Float.log2 @@ Z.to_float z
                                        in
                                        log @@ Z.abs @@ Z.sub z pivot
                                    in
                                    Float.abs ((log itv.Interval.hi) -. (log itv.lo))
                                )
                            in
                            let itv_score_proximity_itv pivot_itv itv =
                                let lo = pivot_itv.Interval.lo in
                                let middle = Z.div (Z.add pivot_itv.hi pivot_itv.lo) @@ Z.of_int 2 in
                                let hi = Z.sub pivot_itv.hi Z.one in
                                let do_single itv =
                                    if middle <= itv.Interval.lo
                                    then itv_score_proximity hi itv
                                    else itv_score_proximity lo itv
                                in
                                if Interval.contains itv middle
                                then 
                                (
                                    let imiddle = Interval.create ~lo:middle ~hi:(Z.add middle Z.one) in
                                    match Interval.split middle itv with
                                    | [a; b] -> (itv_score_proximity lo a) +. (itv_score_proximity hi b) +. (itv_score_proximity lo imiddle)
                                    | [a] -> (do_single a) +. itv_score_proximity lo imiddle
                                    | [] -> itv_score_proximity lo imiddle
                                    | _ -> assert false
                                )
                                else do_single itv
                            in
                            let itv_score_uniform itv =
                                Z.to_float @@ Interval.card itv
                            in
                            let basic = lazy
                                (
                                    let basescore = do_score itv_score_uniform map.range basedomain in
                                    let sizescore = do_score (itv_score_proximity Z.zero) sizemaxitv sizedomain in
                                    Some(basescore, sizescore)
                                )
                            in
                            match r.detection.result.reason, obji with
                            | TagOOB(_), Some(obji) ->
                            (
                                if Interval.subset obji map.range
                                then
                                (
                                    let _, base_outside_obj = Symbolic.Properties.Sns.ResVal.cut obji basedomain in
                                    let sizedomain_oob, size_pivot =
                                        if base_outside_obj.Symbolic.Properties.Sns.ResVal.itvs = []
                                        then 
                                        (
                                            let max_base = List.fold_left
                                                (
                                                    fun max (itv, _) ->
                                                        let itv = Symbolic.Properties.Sns.get_itv itv in
                                                        if max < itv.Interval.hi
                                                        then Z.sub itv.hi Z.one
                                                        else max
                                                )
                                                Z.zero basedomain.Symbolic.Properties.Sns.ResVal.itvs
                                            in
                                            let _, sizedomain_oob = Symbolic.Properties.Sns.ResVal.cut (Interval.create ~lo:Z.zero ~hi:(Z.sub obji.hi max_base)) sizedomain in
                                            sizedomain_oob, Z.sub (Z.sub obji.hi max_base) Z.one
                                        )
                                        else sizedomain, Z.zero
                                    in
                                    let basedomain_oob =
                                        let max_size = List.fold_left
                                            (
                                                fun max (itv, _) ->
                                                    let itv = Symbolic.Properties.Sns.get_itv itv in
                                                    if max < itv.Interval.hi
                                                    then Z.sub itv.hi Z.one
                                                    else max
                                            ) 
                                            Z.zero sizedomain_oob.Symbolic.Properties.Sns.ResVal.itvs
                                        in
                                        if max_size < Z.sub obji.hi obji.lo
                                        then
                                        (
                                            let _, basedomain_oob = Symbolic.Properties.Sns.ResVal.cut (Interval.create ~lo:obji.Interval.lo ~hi:(Z.sub obji.Interval.hi max_size)) basedomain in
                                            basedomain_oob
                                        )
                                        else basedomain
                                    in
                                    let basescore = do_score (itv_score_proximity_itv obji) map.range basedomain_oob in
                                    let sizescore = do_score (itv_score_proximity size_pivot) sizemaxitv sizedomain_oob in
                                    Some(basescore, sizescore)
                                )
                                else Lazy.force basic
                            )
                            | _ -> Lazy.force basic
                        )
                    in
                    let final base size data =
                        let mean l =
                            let acc, div = List.fold_left (fun (acc, div) t -> Triple.apply_triple (+.) acc t, div +. 1.0) (Triple.zero, 0.0) l in
                            Triple.apply_float (/.) acc div
                        in
                        match data with
                        | Some(data) -> mean [base; size; data]
                        | _ -> mean [base; size]
                    in
                    let mk_score map basedomain sizedomain data =
                        match rangescore map basedomain sizedomain with
                        | Some(base, size) ->
                        (
                            let overall = final base size data in
                            Some({base; size; data; overall})
                        )
                        | _ -> None
                    in
                    let mk_scores basedomain sizedomain datadomains =
                        let data = 
                            match datadomains with
                            | [] -> None
                            | _ -> Some(datascore datadomains)
                        in
                        List.fold_left
                        (
                            fun res (_, map) ->
                                match mk_score map basedomain sizedomain data with
                                | Some(score) -> MapMap.add map score res
                                | None -> res
                        )
                        MapMap.empty r.maps
                    in
                    (*let op_scores op a b =
                        let base = Triple.apply_triple op a.base b.base in
                        let size = Triple.apply_triple op a.size b.size in
                        let data =
                            match a.data, b.data with
                            | Some(a), Some(b) -> Some(Triple.apply_triple op a b)
                            | None, None -> None
                            | _ -> assert false
                        in
                        let overall = final base size data in
                        {base; size; data; overall}
                    in*)
                    match r.base, r.size with
                    | Some(base), Some(size) ->
                    (
                        let basedomain = base.domain in
                        let sizedomain = size.domain in
                        let datadomains = List.map (fun p -> p.domain) r.data in

                        let alone = mk_scores basedomain sizedomain datadomains in

                        let similar, full =
                            match similar with
                            | [] -> [], None
                            | _ ->
                            (
                                let combine ?(shift = None) filter domain old_domains =
                                    let v = Binsec.Formula.bv_var "bv" domain.Result.result.Symbolic.Properties.Sns.ResVal.size in
                                    let vdecl = Binsec.Formula.mk_bv_decl v [] in
                                    let vterm = Binsec.Formula.mk_bv_fun v [] in
                                    let domainconstr = Symbolic.Properties.Sns.ResVal.constr_builder domain.Result.result (Binsec.Formula.BvVar(v)) [] in
                                    let defs, constr = 
                                        match shift with
                                        | Some(shifts) -> 
                                        (
                                            let cnt = ref 0 in
                                            let with_shift (defs, res) domain shift =
                                                let w = Binsec.Formula.bv_var (Printf.sprintf "shifted_%d" !cnt) v.Binsec.Formula.bv_size in
                                                cnt := !cnt + 1;
                                                let wbody = 
                                                    if shift > Z.zero
                                                    then Binsec.Formula.mk_bv_add vterm @@ Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create shift v.Binsec.Formula.bv_size
                                                    else Binsec.Formula.mk_bv_sub vterm @@ Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create (Z.abs shift) v.Binsec.Formula.bv_size
                                                in
                                                let wdef = Binsec.Formula.mk_bv_def w [] wbody in
                                                let constr = Symbolic.Properties.Sns.ResVal.constr_builder domain.Result.result (Binsec.Formula.BvVar(w)) [] in
                                                wdef::defs, Binsec.Formula.mk_bl_or res constr
                                            in
                                            List.fold_left2 with_shift ([], domainconstr) old_domains shifts
                                        )
                                        | None -> [], List.fold_left
                                            (
                                                fun res domain ->
                                                    let constr = Symbolic.Properties.Sns.ResVal.constr_builder domain.Result.result (Binsec.Formula.BvVar(v)) [] in
                                                    Binsec.Formula.mk_bl_or res constr
                                            )
                                            domainconstr old_domains
                                    in
                                    let proj, projs = Symbolic.StateFormula.Projections.add ~suff:"proj" "bv" true Symbolic.StateFormula.Projections.empty in
                                    let projv = Binsec.Formula.bv_var (Symbolic.StateFormula.Projections.Projection.pp proj) domain.Result.result.Symbolic.Properties.Sns.ResVal.size in
                                    let projdef = Binsec.Formula.mk_bv_def projv [] vterm in
                                    Binsec.Formula.empty
                                        |> Binsec.Formula.push_front_declare vdecl
                                        |> List.fold_right Binsec.Formula.push_front_define defs
                                        |> Binsec.Formula.push_front_define projdef
                                        |> Binsec.Formula.push_front_assert constr
                                        |> Symbolic.StateFormula.create ~projections:projs
                                        |> Symbolic.Properties.Sns.sns proj
                                in
                                let treat_weak itv =
                                    let itv = Symbolic.Properties.Sns.get_itv itv in
                                    let lo = Interval.create ~lo:itv.Interval.lo ~hi:(Z.add Z.one itv.Interval.lo) in
                                    let hi = Interval.create ~lo:(Z.sub itv.Interval.hi Z.one) ~hi:itv.Interval.hi in
                                    [Symbolic.Properties.Sns.Strong(lo); Strong(hi)]
                                in
                                let strongfilter itv =
                                    match itv with
                                    | Symbolic.Properties.Sns.Strong(_) -> [itv]
                                    | MaybeStrong(_)
                                    | NotStrong(_) -> treat_weak itv
                                    | Unknown(_) -> []
                                in
                                let maybefilter itv =
                                    match itv with
                                    | Symbolic.Properties.Sns.Strong(_)
                                    | MaybeStrong(_) -> [itv]
                                    | NotStrong(_) -> treat_weak itv
                                    | Unknown(_) -> []
                                in
                                let weakfilter itv = [itv] in
                                let ddl = List.map (fun _ -> []) datadomains in
                                let shift, shiftref =
                                    match r.detection.Result.result.Oob_check.ResVal.reason with
                                    | Oob_check.ResVal.TagOOB(tt)
                                    | TagUAF(tt) -> Some([]), Some(tt.Oob_check.TT.base)
                                    | _ -> None, None
                                in
                                let similar, shift, bdl, sdl, ddl = List.fold_left
                                    (
                                        fun (similar, shift, bdl, sdl, ddl) (file, (old : t)) ->
                                            let id = old.detection.Result.result.Oob_check.ResVal.id in
                                            let nshift =
                                                match shift, shiftref, old.detection.Result.result.Oob_check.ResVal.reason with
                                                | Some(shift), Some(addr), TagOOB(tt)
                                                | Some(shift), Some(addr), TagUAF(tt) -> 
                                                (
                                                    let nshift = Z.sub tt.Oob_check.TT.base addr in
                                                    Some(Some(nshift::shift))
                                                )
                                                | Some(_), Some(_), _ -> None
                                                | _ -> Some(None)
                                            in
                                            match nshift, old.base, old.size with
                                            | Some(nshift), Some(ob), Some(os) ->
                                            (
                                                let obd = ob.domain in
                                                let osd = os.domain in
                                                let odd = List.map (fun p -> p.domain) old.data in
                                                (id, file)::similar,
                                                nshift,
                                                obd::bdl,
                                                osd::sdl,
                                                List.map2 (fun dd ddl -> dd::ddl) odd ddl
                                            )
                                            | _ -> similar, shift, bdl, sdl, ddl
                                    )
                                    ([], shift, [], [], ddl) similar
                                in
                                let get_scores filter =
                                    let nbd = combine ~shift filter basedomain bdl in
                                    let nsd = combine filter sizedomain sdl in
                                    let ndd = List.map2 (combine filter) datadomains ddl in
                                    mk_scores nbd nsd ndd
                                in
                                let strongscores = get_scores strongfilter in
                                let maybescores = get_scores maybefilter in
                                let weakscores = get_scores weakfilter in
                                let recombine map _ =
                                    let recombine strong maybe weak =
                                        let min = strong.Triple.min in
                                        let maybe_min = maybe.Triple.max in
                                        let max = weak.Triple.max in
                                        {Triple.min; maybe_min; max}
                                    in
                                    try
                                        let strongscores = MapMap.find map strongscores in
                                        let maybescores = MapMap.find map maybescores in
                                        let weakscores = MapMap.find map weakscores in
                                        let base = recombine strongscores.base maybescores.base weakscores.base in
                                        let size = recombine strongscores.size maybescores.size weakscores.size in
                                        let data = 
                                            match strongscores.data, maybescores.data, weakscores.data with
                                            | Some(strong), Some(maybe), Some(weak) -> Some(recombine strong maybe weak)
                                            | _ -> None
                                        in
                                        let overall = recombine strongscores.overall maybescores.overall weakscores.overall in
                                        {base; size; data; overall}
                                    with Not_found -> assert false
                                in
                                let full = MapMap.mapi recombine strongscores in
                                similar, Some(full)
                            )
                        in
                        (*let similar, full, _ = 
                            match similar with
                            | [] -> [], None
                            | _ -> List.fold_left
                                (
                                    fun (similar, full, dl) (file, old) ->
                                        let id = report.detection.Result.result.Oob_check.ResVal.id in
                                        match old.base, old.size with
                                        | Some(ob), Some(os) ->
                                        (
                                            let obd = ob.domain in
                                            let osd = os.domain in
                                            let odd = List.map (fun p -> p.domain) old.data in

                                            let old_alone = mk_scores obd osd odd in

                                            let interscores = List.fold_left (fun 
                                                @@ List.map (fun (bd, sd, dd) -> mk_scores bd sd dd)
                                                @@ List.map
                                                (
                                                    fun (bd, sd, dd) ->
                                                        let do_inter a b = Symbolic.Properties.Sns.Res.create ~label:"aaa" @@ Symbolic.Properties.Sns.ResVal.intersect a.Result.result b.result.result in
                                                        do_inter obd bd,
                                                        do_inter osd sd,
                                                        List.map2 do_inter odd dd
                                                )
                                            in
                                        )
                                        | _ -> similar, full, (bdl, sdl, ddl)
                                )
                                ([], alone, [basedomain, sizedomain, datadomains])
                                similar
                        in*)
                        let report = {alone; similar; full} in
                        Some(Default(report), lazy (to_generic_default_report report))
                    )
                    | _ -> None

                let _ =
                    Reporters.register "default" "Compute a score for each memory mapping that can be hit. Data scores are computed as |domain| / |2^size|. The scores for bases / sizes are computed with weighted quantitative control, with the weight function 1 / (log(2) * d), d being the distance between the parameter and the nearest buffer bound / overflow size, when applicable (i.e., when there is a known valid targeted buffer). Otherwise, base scores are computed the same way as data scores and size scores are computed with weighted quantitative control with the same weight function, except that they are passed directly instead of the distance d. This ensures that scores are biased toward reads / writes close to the buffer / targeted location. The overall score is the mean of all these scores." create_default_report
            end
    end

module Res = Result.Make (ResVal)

let capas_from_generic_results res =
    Res.create ~label:res.Result.label @@ ResVal.of_generic res.result

module S =
    struct
        type t =
            {
                symb : Symbolic_policy.SA.t;
                results : (bool * ResVal.t) Utils.IntMap.t;
                dont_update : bool;
                reporter : string;
                selected_detections : int list;
                old_results : Res.result list Utils.StringMap.t
            }

        let create ?(tag = None) () =
            let symb = Symbolic_policy.init_analysis ~tag "Symbolic" in
            let results = Utils.IntMap.empty in
            {symb; results; dont_update = false; reporter = "default"; selected_detections = []; old_results = Utils.StringMap.empty}

        let select_detections selected_detections s =
            {s with selected_detections}

        let set_reporter reporter s =
            {s with reporter}

        let load_old_results file =
            let res = Result.load_from_file file in
            let exp = Str.regexp {|OOB Capabilities <[^>]+>$|} in
            try
                let res = 
                    let find r =
                        try
                            ignore @@ Str.search_forward exp r.Result.label 0;
                            true
                        with Not_found -> false
                    in
                    match res.Result.result with
                    | Result.Res(l) -> List.find find l
                    | _ -> assert false
                in
                match res.Result.result with
                | Result.Res(l) -> List.map capas_from_generic_results l
                | _ -> assert false
            with Not_found -> []
            | e -> raise (Failure(Printf.sprintf "could not load OOB capabilities from file <%s> (%s)" (File.get_path file) @@ Printexc.to_string e))

        let load files s =
            List.fold_left
                (
                    fun s file ->
                        if not ((File.get_name file) = "")
                        then
                        (
                            let loaded = load_old_results file in
                            Message.Wrap.send (Message.Wrap.Debug("OOBCAPA", lazy (Printf.sprintf "Loading OOB capabilities from %s" @@ File.get_path file)));
                            List.iter (fun res -> Message.Wrap.send (Message.Wrap.Debug("OOBCAPA", lazy (Result.pp @@ Res.export res)))) loaded;
                            {s with old_results = Utils.StringMap.add (File.get_path file) loaded s.old_results}
                        )
                        else s
                )
                s files

        let find_or_create_result detec s =
            let id = detec.Result.result.Oob_check.ResVal.id in
            let res = lazy
                (
                    try
                        let _, res = Utils.IntMap.find id s.results in
                        res
                    with Not_found -> ResVal.create detec
                )
            in
            if s.selected_detections = []
            then Some(Lazy.force res)
            else
            (
                try
                    ignore @@ List.find (fun i -> i = id) s.selected_detections;
                    Some(Lazy.force res)
                with Not_found -> None
            )

        let add_result res s =
            let id = res.ResVal.detection.Result.result.Oob_check.ResVal.id in
            let dont_update = not res.detection.result.valid in
            try
                let fresh, _ = Utils.IntMap.find id s.results in
                {s with results = Utils.IntMap.add id (fresh, res) s.results; dont_update}
            with Not_found -> {s with results = Utils.IntMap.add id (true, res) s.results; dont_update}

        let mk_report id res s =
            let similar = Utils.StringMap.fold
                (
                    fun file reports sim ->
                        sim @ List.fold_left
                            (
                                fun sim report ->
                                    if Oob_check.ResVal.similar res.ResVal.detection.Result.result report.Result.result.ResVal.detection.Result.result
                                    then (Message.Wrap.send (Message.Wrap.BigInfo(Printf.sprintf "Similar detec %d from %s" report.Result.result.detection.Result.result.Oob_check.ResVal.id file)); (file, report.Result.result)::sim)
                                    else sim
                            )
                            [] reports
                )
                s.old_results []
            in
            let res = ResVal.create_report ~similar s.reporter res in
            let label = Printf.sprintf "Detection %d" id in
            Res.send ~prefix:"RESULT" @@ Res.create ~label res

        let maybe_finalize_results trace s =
            let aux id (fresh, res) results =
                if fresh
                then Utils.IntMap.add id (false, res) results
                else
                (
                    mk_report id res s;
                    Utils.IntMap.remove id results
                )
            in
            match trace with
            | Trace.Ins(_) -> {s with results = Utils.IntMap.fold aux s.results s.results}
            | _ -> s

        let force_finalize_results s =
            Utils.IntMap.iter (fun id (_, res) -> mk_report id res s) s.results;
            {s with results = Utils.IntMap.empty}

        let add_stats_to_bundle b _ = b
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

type t = A.t

let analyze snk a =
    let state = A.get_state a in
    let domain = lazy
        (
            match Symbolic_policy.SB.get_sink_formula snk @@ Symbolic_policy.SA.get_bank state.symb with
            | Some(proj, sf) -> Symbolic.Properties.Sns.sns proj sf
            | None -> assert false
        )
    in
    let entry = lazy {ResVal.snk; domain = Lazy.force domain} in
    match snk.Sink.kind with
    | Auto(Oob_auto_sink.OOBBase(detec) as kind, _) 
    | Auto(Oob_auto_sink.OOBSize(detec) as kind, _) 
    | Auto(Oob_auto_sink.OOBWData(detec) as kind, _)    
    | Auto(Oob_auto_sink.OOBWDataNext(detec) as kind, _) ->
    (
        match kind, S.find_or_create_result detec state with
        | Oob_auto_sink.OOBBase(_), Some(res) -> 
        (
            let base = Some(Lazy.force entry) in
            let offset =
                match detec.Result.result.Oob_check.ResVal.reason with
                | Oob_check.ResVal.TagOOB(tt)
                | TagUAF(tt) ->
                (
                    let lo, hi = tt.Oob_check.TT.base, Z.sub (Z.add tt.base @@ Z.of_int tt.size) Z.one in
                    let expr = Expr.Bnop
                        (
                            Expr.Min, 
                            Expr.Bnop
                                (
                                    Expr.Dist,
                                    snk.Sink.expr,
                                    Expr.Const(lo, 8)
                                )
                            ,
                            Expr.Bnop
                                (
                                    Expr.Dist,
                                    snk.Sink.expr,
                                    Expr.Const(hi, 8)
                                )
                        )
                    in
                    let offsnk = Sink.create ~dummy:true ~fname:snk.Sink.fname (Printf.sprintf "Detection_%d_OOB_offset" detec.Result.result.Oob_check.ResVal.id) expr in
                    let domain =
                        match Symbolic_policy.SB.get_sink_formula offsnk @@ Symbolic_policy.SA.get_bank state.symb with
                        | Some(proj, sf) -> Symbolic.Properties.Sns.sns proj sf
                        | None -> assert false
                    in
                    Some({ResVal.snk = offsnk; domain})
                )
                | _ -> None
            in
            A.set_state a @@ S.add_result {res with base; offset} state
        )
        | Oob_auto_sink.OOBSize(_), Some(res) -> 
                A.set_state a @@ S.add_result {res with size = Some(Lazy.force entry)} state
        | Oob_auto_sink.OOBWData(_), Some(res)
        | Oob_auto_sink.OOBWDataNext(_), Some(res) -> 
                A.set_state a @@ S.add_result {res with data = res.data @ [Lazy.force entry]} state
        | _ -> a
    )
    | _ -> a

let callback trace a =
    let update_symb a =
        let state = A.get_state a in
        A.set_state a {state with symb = Symbolic_policy.SA.analyze trace state.symb}
    in
    match trace with
    | Trace.Snk(snk) -> update_symb @@ analyze snk a
    | Ins(_) 
    | Fun(_) ->
    (
        let state = S.maybe_finalize_results trace @@ A.get_state a in
        if state.S.dont_update
        then A.set_state a state
        else A.set_state a {state with symb = Symbolic_policy.SA.analyze trace state.symb}
    )
    | _ -> update_symb a

let end_callback signal a =
    let s = S.force_finalize_results @@ A.get_state a in
    A.set_state a {s with symb = Symbolic_policy.SA.fini signal s.symb}

let init_analysis ?(tag = None) ?selected_detections ?reporter ?load name =
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
    let load = List.map File.from_file @@ String.split_on_char ';' @@ load_opt#maybe_get ?tag load in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = A.set_state a 
        @@ S.set_reporter reporter
        @@ S.select_detections selected_detections 
        @@ S.load load
        @@ A.get_state a 
    in
    let todo a =
        a
            |> A.TraceCallbacks.add "analysis" callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "oob capa start" (A.FunctionCallbacks.start_callback ~name:"oob capa start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "oobcapa"
        let desc = "Analyze OOB read / write capabilities."

        let init ?(tag = None) () =
            init_analysis ~tag "OOB Capabilities"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option "oobcapa" "-select-detections" (Options.TaggedOptions.String(sdetec_opt)) "Only analyze specified detections (format: 1;2;10;...).";
    Options.register_policy_alias "oobcapa" "-select-detections" "-sd";
    Options.register_policy_option "oobcapa" "-reporter" (Options.TaggedOptions.String(reporter_opt)) "Select an OOB capability report style.";
    Options.register_policy_option "oobcapa" "-load" (Options.TaggedOptions.String(load_opt)) "Load capabilities from previous runs and attempt to merge them with new ones. Detections are deemed similar if they occur in the same location and are of the same type. For OOBs and UAFs relative to a known object, loaded base address capabilities are shifted to match the object in the current run (thus nullifying the effects of ASLR), but, importantly, this does not work if bases are not derived from a pointer to a known object (then full scores for bases may be unreliable when combining multiple runs due to ASLR)."
