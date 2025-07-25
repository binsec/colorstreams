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

let check_range proj i sf =
    let mk_constr v args =
        match v with
        | Binsec.Formula.BvVar(v) ->
        (
            let bv = Binsec.Formula.mk_bv_fun v args in
            StateFormula.FormulaUtils.interval i bv
        )
        | _ -> Binsec.Formula.mk_bl_true
    in
    let sf = StateFormula.add_proj_constr proj mk_constr sf in
    match Sat.check proj sf with
    | Property.OK(Result.Custom((StateFormula.Status.CustomResult(status)), _)) -> sf, Some(status)
    | _ -> sf, None

let check_point proj point sf =
    let mk_constr v args =
        match v with
        | Binsec.Formula.BvVar(v) ->
        (
            let bv = Binsec.Formula.mk_bv_fun v args in
            let cst = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create point bv.Binsec.Formula.bv_term_size in
            Binsec.Formula.mk_bv_equal bv cst
        )
        | _ -> Binsec.Formula.mk_bl_true
    in
    let sf = StateFormula.add_proj_constr proj mk_constr sf in
    match Sat.check proj sf with
    | Property.OK(Result.Custom((StateFormula.Status.CustomResult(status)), _)) -> sf, Some(status)
    | _ -> sf, None

let rec bin_search_bound lo proj i sf =
    if Interval.card i = Z.one
    then i.lo
    else
    (
        let loi, hii = 
            match Interval.halve i with
            | [loi; hii] -> loi, hii
            | _ -> raise Not_found
        in
        let statuslo = 
            match check_range proj loi sf with
            | _, Some(status) -> status
            | _ -> raise Not_found
        in
        let statushi = 
            match check_range proj hii sf with
            | _, Some(status) -> status
            | _ -> raise Not_found
        in
        match statuslo.Result.result, statushi.Result.result with
        | StateFormula.StatusVal.Sat(_), StateFormula.StatusVal.Sat(_) ->
        (
            if lo
            then bin_search_bound lo proj loi sf
            else bin_search_bound lo proj hii sf
        )
        | Sat(_), _ -> bin_search_bound lo proj loi sf
        | _, Sat(_) -> bin_search_bound lo proj hii sf
        | _, _ -> raise Not_found
    )

let bin_search_lower_bound = bin_search_bound true
let bin_search_upper_bound = bin_search_bound false

let density_lim = ref 64

let density lim proj i sf =
    let rec sample hits misses todo =
        if todo <= 0
        then hits, misses
        else
        (
            let point = Interval.pick_random i in
            match check_point proj point sf with
            | sf, Some({Result.result = StateFormula.StatusVal.Sat(_); _}) -> sample (hits + 1) misses (todo - 1)
            | _, _ -> sample hits (misses + 1) (todo - 1)
        )
    in
    let hits, misses = sample 0 0 lim in
    let hits = Float.of_int hits in
    let misses = Float.of_int misses in
    let tot = hits +. misses in
    let wilson_base = (hits +. 0.5 *. 1.96 *. 1.96) /. (tot +. 1.96 *. 1.96) in
    let wilson_err = (1.96 /. (tot +. 1.96 *. 1.96)) *. Float.sqrt (hits *. misses /. tot +. 1.96 *. 1.96 /. 4.) in
    wilson_base -. wilson_err, wilson_base +. wilson_err

type res = Point of Z.t | Density of Interval.t * Float.t * Float.t

module ResVal =
    struct
        type t = 
            {
                feasible : res list;
                exact : Z.t;
                range : Z.t * Z.t
            }

        let create res =
            let exact, lo, hi = List.fold_left 
                (
                    fun (exact, lo, hi) e ->
                    (
                        match e with
                        | Point(_) -> (Z.add Z.one exact), (Z.add Z.one lo), (Z.add Z.one hi)
                        | Density(i, dlo, dhi) ->
                        (
                            let card = Z.to_float @@ Interval.card i in
                            let lo = Z.add lo @@ Z.of_float (card *. dlo) in
                            let hi = Z.add hi @@ Z.of_float (card *. dhi) in
                            exact, lo, hi
                        ) 
                    )
                )
                (Z.zero, Z.zero, Z.zero) res
            in
            {feasible = res; exact; range = lo, hi}

        let to_generic res =
            let pcnt = ref 0 in
            let icnt = ref 0 in
            let reslist = List.map
                (
                    fun e ->
                    (
                        match e with
                        | Point(p) ->
                        (
                            let label = Printf.sprintf "P%d" !pcnt in
                            pcnt := !pcnt + 1;
                            Result.mk_int ~label p
                        )
                        | Density(i, lo, hi) ->
                        (
                            let label = Printf.sprintf "I%d" !icnt in
                            icnt := !icnt + 1;
                            let lo = Result.Flt(lo) in
                            let hi = Result.Flt(hi) in
                            let i = Result.mk_interval ~label:"interval" i in
                            let range = Result.mk_list ~label:"density" [lo; hi] in
                            Result.mk_res_list ~label [i; range]
                        )
                    )
                )
                res.feasible
            in
            let exact = Result.mk_int ~label:"exact" res.exact in
            let lo, hi = res.range in
            let prob = Result.mk_list ~label:"probabilistic" [Result.Int(lo); Result.Int(hi)] in
            let tot = Result.mk_res_list ~label:"Total" [exact; prob] in
            Result.Res(reslist @ [tot])
    end

module Res = Result.Make (ResVal)

let exhaust_lim = ref 64
let timeout = ref (-1)

let newsome ?(exhaust_lim = !exhaust_lim) ?(density_lim = !density_lim) ?(timeout = !timeout) proj sf =
    let proj_def = 
        try
            StateFormula.Projections.find proj @@ StateFormula.get_projections sf
        with Not_found -> assert false
    in
    let proj_var =
        match StateFormula.FormulaUtils.def_var_args proj_def with
        | BvVar(bv), _ -> bv
        | _, _ -> assert false
    in
    let proj_size = proj_var.Binsec.Formula.bv_size in
    
    let timer = new Stats.timer "runtime" "Runtime" in
    let timeout_flag = new Stats.flag "timeout" "Timeout" in
    let stats = Stats.Bundle.create ""
        |> Stats.Bundle.add_stat (timer :> Stats.stat)
        |> Stats.Bundle.add_stat (timeout_flag :> Stats.stat)
    in

    let rec aux stop splits todo =
        match todo with
        | h::t ->
        (
            if timeout > 0 && timer#get > (Float.of_int timeout)
            then
            (
                if not stop 
                then
                ( 
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy "newsome: timeout"));
                    timeout_flag#set;
                )
                ;
                (Density(h, 0., 1.))::(aux true splits t)
            )
            else
            (
                let sf_, status = check_range proj h sf in
                match status with
                | Some({Result.result = (StateFormula.StatusVal.Sat([(_, pivot)])); _}) ->
                (
                    let lo =
                        match check_point proj h.Interval.lo sf with
                        | _, Some({Result.result = StateFormula.StatusVal.Sat(_); _}) -> h.lo
                        | _ -> 
                        (
                            try
                                bin_search_lower_bound proj h sf
                            with Not_found -> h.lo
                        )
                    in
                    let hi =
                        match check_point proj (Z.sub h.Interval.hi Z.one) sf with
                        | _, Some({Result.result = StateFormula.StatusVal.Sat(_); _}) -> h.hi
                        | _ -> 
                        (
                            try
                                Z.add Z.one @@ bin_search_upper_bound proj h sf
                            with Not_found -> h.hi
                        )
                    in
                    let h = Interval.create ~lo ~hi in
                    if splits <= 0
                    then
                    (
                        let dlo, dhi = density density_lim proj h sf in
                        (Density(h, dlo, dhi))::(aux stop splits t)
                    )
                    else
                    (
                        try
                            let l = Interval.split pivot h in
                            if splits = 1 then Message.Wrap.send (Message.Wrap.Warning(lazy "newsome: max splits reached"));
                            (Point(pivot))::(aux stop (splits - 1) (t @ l))
                        with Failure e ->
                        (
                            Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "newsome: split failed (%s)" e)));
                            aux stop splits t
                        )
                    )
                )
                | Some({result = Unsat; _}) -> aux stop splits t
                | _ ->
                (
                    Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "newsome: error when checking sat in %s" @@ Interval.pp h)));
                    File.write (StateFormula.pp sf_) @@ File.create ~keep:true (proj_var.bv_name ^"_newsome_sat_err") "smt2";
                    (Density(h, 0., 1.))::(aux stop splits t)
                )
            )
        )
        | _ -> []
    in
    let lo = Z.zero in
    let hi = Z.pow (Z.of_int 2) proj_size in
    timer#start;
    let res = aux false exhaust_lim [Interval.create ~lo ~hi] in
    timer#stop;
    Property.mk_ok "Newsome" proj None @@ Res.wrap @@ Res.create ~label:"Newsome" ~stats:(Some(stats)) @@ ResVal.create res

let _ =
    Options.register_option ("-newsome-l", Arg.Set_int exhaust_lim, "Maximum number of splits allowed during feasible value set evaluation.");
    Options.register_option ("-newsome-dl", Arg.Set_int density_lim, "Sample count for density measure during feasible value set evaluation.");
    Options.register_option ("-newsome-t", Arg.Set_int timeout, "Set timeout (s).");
    Property.register "Newsome" "feasible value set" newsome None
