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

let split_max = ref [-1]
(*let halve = ref false*)
let timeout = ref (-1)
let pmc_solver = ref ""
let force_binsearch_bounds = ref false

let parse_splits s =
    split_max := List.fold_right
        (
            fun n res ->
                try
                    (int_of_string n)::res
                with _ -> res
        ) 
        (String.split_on_char ';' s) []
    ;
    if !split_max = [] then split_max := [-1]

type itv = Strong of Interval.t | MaybeStrong of Interval.t | NotStrong of Interval.t | Unknown of Interval.t

type itv_half = Left | Right | NA

let mark_halves l =
    match l with
    | [left; right] -> [(left, Left); (right, Right)]
    | [i] -> [(i, NA)]
    | _ -> assert false

let get_itv = function
    | Strong(itv)
    | MaybeStrong(itv)
    | NotStrong(itv)
    | Unknown(itv) -> itv

let of_itv_kind i = function
    | Strong(_) -> Strong(i)
    | MaybeStrong(_) -> MaybeStrong(i)
    | NotStrong(_) -> NotStrong(i)
    | Unknown(_) -> Unknown(i)

module Constraints =
    struct
        let default = ref ["none"]

        let constrs = ref Utils.StringMap.empty
        let suffs = ref Utils.StringMap.empty

        type t = StateFormula.Projections.Projection.t -> StateFormula.t -> StateFormula.t * Result.wrapped_result Property.ok option

        let register name desc suff f g from_generic intersect =
            if Utils.StringMap.mem name !constrs
            then raise (Failure (Printf.sprintf "S&S constraints: name <%s> already in use" name))
            else if Utils.StringMap.mem suff !suffs
            then raise (Failure (Printf.sprintf "S&S constraints: suffix <%s> already in use" suff))
            else 
            (
                suffs := Utils.StringMap.add suff true !suffs;
                constrs := Utils.StringMap.add name (name, desc, suff, f, g, from_generic, intersect) !constrs
            )

        let pp () =
            let s = Utils.StringMap.fold (fun _ (name, desc, _, _, _, _, _) res -> Printf.sprintf "%s\n- %s: %s" res name desc) !constrs "Available S&S additional constraints:" in
            Message.Wrap.send (Message.Wrap.BigInfo(s))

        let get name =
            try
                let _, _, _, mk_constr, _, _, _ = Utils.StringMap.find name !constrs in
                mk_constr
            with Not_found -> raise (Failure(Printf.sprintf "S&S constraints: unknown constraint <%s>" name))

        let get_constr_builder name =
            try
                let _, _, _, _, mk_constr, _, _ = Utils.StringMap.find name !constrs in
                mk_constr
            with Not_found -> raise (Failure(Printf.sprintf "S&S constraints: unknown constraint <%s>" name))

        let get_parser name =
            try
                let _, _, _, _, _, from_generic, _ = Utils.StringMap.find name !constrs in
                from_generic
            with Not_found -> raise (Failure(Printf.sprintf "S&S constraints: unknown constraint <%s>" name))

        let get_intersect name =
            try
                let _, _, _, _, _, _, intersect = Utils.StringMap.find name !constrs in
                intersect
            with Not_found -> raise (Failure(Printf.sprintf "S&S constraints: unknown constraint <%s>" name))

        let get_suff name =
            try
                let _, _, suff, _, _, _, _ = Utils.StringMap.find name !constrs in
                suff
            with Not_found -> raise (Failure(Printf.sprintf "S&S constraints: unknown constraint <%s>" name))

        let _ =
            register "None" "No additional constraints." "" (fun _ sf -> sf, None) (fun _ -> None) (fun _ -> None) (fun _ _ -> None);
            Options.register_option ("-sns-constr", Arg.String(fun s -> default := String.split_on_char ';' s), "Select additional constraints to take into account for Shrink and Split (format: constr1;constr2).");
            Options.register_option ("-sns-list-constrs", Arg.Unit(fun () -> Options.register_todo pp), "List available additional constraints for Shrink and Split.")
    end

module ResVal =
    struct
        type t =
            {
                constrprop : string;
                constrres : Result.wrapped_result Property.ok option;
                size : int;
                card : itv -> Interval.t Lazy.t;
                itvs : (itv * Interval.t Lazy.t) list;
                min : Z.t Lazy.t;
                maybe_min : Z.t Lazy.t;
                max : Z.t Lazy.t
            }

        let create_aux constrprop constrres size card itvs =
            let itvs = List.map (fun itv -> itv, card itv) itvs in
            let counts = lazy 
                (
                    List.fold_left
                        (
                            fun (min, maybe_min, max) (itv, card) -> 
                            (
                                let card = Lazy.force card in
                                let hi = Z.sub card.Interval.hi Z.one in
                                (Z.add min card.Interval.lo),
                                (
                                    match itv with
                                    | MaybeStrong(_) -> Z.add maybe_min hi
                                    | _ -> Z.add maybe_min card.Interval.lo
                                ),
                                (Z.add max hi)
                            )
                        )
                        (Z.zero, Z.zero, Z.zero) itvs
                )
            in
            let min = lazy (match Lazy.force counts with min, _, _ -> min) in
            let maybe_min = lazy (match Lazy.force counts with _, maybe_min, _ -> maybe_min) in
            let max = lazy (match Lazy.force counts with _, _, max -> max) in
            {constrprop; constrres; size; card; itvs; min; maybe_min; max}

        let create_no_pmc constrprop constrres size itvs =
            let card i =
                let do_card i =
                    match constrres with
                    | Some(constrres) ->
                    (
                        let mk_constr =
                            let constr_builder = Constraints.get_constr_builder constrprop in
                            match constr_builder constrres with
                            | Some(f) -> f
                            | _ -> fun _ _ -> Binsec.Formula.mk_bl_true
                        in
                        let x = Binsec.Formula.bv_var "x_proj" size in
                        let x_decl = Binsec.Formula.mk_bv_decl x [] in
                        let x_term = Binsec.Formula.bv_term (Binsec.Formula.BvFun(x, [])) in

                        let lo = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create i.Interval.lo size in
                        let hi = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create (Z.sub i.Interval.hi Z.one) size in
                        let leq = Binsec.Formula.mk_bv_comp BvUle lo x_term in
                        let geq = Binsec.Formula.mk_bv_comp BvUge hi x_term in
                        let interval = Binsec.Formula.mk_bl_and leq geq in

                        let constrx = mk_constr (Binsec.Formula.BvVar(x)) [] in

                        Binsec.Formula.empty
                            |> Binsec.Formula.push_front_declare x_decl
                            |> Binsec.Formula.push_front_assert interval
                            |> Binsec.Formula.push_front_assert constrx
                            |> Binsec.Formula_pp.pp_formula Format.str_formatter 
                        ;

                        let file = File.create "interval_card" "smt2" in
                        File.write (Format.flush_str_formatter ()) file;
                        match Pmc.PMCount.Solver.query ~solver:"d4" ~projections:(["x_proj"]) file with
                        | {result = (Pmc.PMCount.CountVal.Count(cnt)); _}::_ -> cnt
                        | _ -> assert false
                    )
                    | None -> Interval.card i
                in
                let mk_sure c =
                    let lo = c in
                    let hi = Z.add c Z.one in
                    Interval.create ~lo ~hi
                in
                let mk_weak c =
                    let lo = min c @@ Z.of_int 2 in
                    let hi = Z.add c Z.one in
                    Interval.create ~lo ~hi
                in
                let mk_idk c =
                    let lo = Z.zero in
                    let hi = Z.add c Z.one in
                    Interval.create ~lo ~hi
                in
                match i with 
                | MaybeStrong(itv)
                | NotStrong(itv) -> mk_weak @@ do_card itv
                | Unknown(itv) -> mk_idk @@ do_card itv
                | Strong(itv) -> mk_sure @@ do_card itv 
            in
            let card itv = lazy (card itv) in
            create_aux constrprop constrres size card itvs

        let create proj proj_def proj_var constrprop constrres sf itvs =
            let proj_size = proj_var.Binsec.Formula.bv_size in
            let card i =
                let do_card i =
                    try
                        let mk_constr = StateFormula.get_proj_constr proj sf in
                        let x = Binsec.Formula.bv_var "x_proj" proj_size in
                        let x_decl = Binsec.Formula.mk_bv_decl x [] in
                        let x_term = Binsec.Formula.bv_term (Binsec.Formula.BvFun(x, [])) in

                        let lo = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create i.Interval.lo proj_size in
                        let hi = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create (Z.sub i.Interval.hi Z.one) proj_size in
                        let leq = Binsec.Formula.mk_bv_comp BvUle lo x_term in
                        let geq = Binsec.Formula.mk_bv_comp BvUge hi x_term in
                        let interval = Binsec.Formula.mk_bl_and leq geq in

                        let constrx = mk_constr (Binsec.Formula.BvVar(x)) [] in

                        Binsec.Formula.empty
                            |> Binsec.Formula.push_front_declare x_decl
                            |> Binsec.Formula.push_front_assert interval
                            |> Binsec.Formula.push_front_assert constrx
                            |> Binsec.Formula_pp.pp_formula Format.str_formatter 
                        ;

                        let file = File.create "interval_card" "smt2" in
                        File.write (Format.flush_str_formatter ()) file;
                        match Pmc.PMCount.Solver.query ~solver:"d4" ~projections:(["x_proj"]) file with
                        | {result = (Pmc.PMCount.CountVal.Count(cnt)); _}::_ -> cnt
                        | _ -> assert false
                    with Not_found -> Interval.card i
                in
                let do_pmc itv =
                    let proj_term = Binsec.Formula.bv_term (Binsec.Formula.BvFun(proj_var, [])) in
                    let lo = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create itv.Interval.lo proj_size in
                    let hi = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create (Z.sub itv.Interval.hi Z.one) proj_size in
                    let leq = Binsec.Formula.mk_bv_comp BvUle lo proj_term in
                    let geq = Binsec.Formula.mk_bv_comp BvUge hi proj_term in
                    let itvconstr = Binsec.Formula.mk_bl_and leq geq in
                    StateFormula.get_base sf
                        |> Binsec.Formula.push_front_assert itvconstr
                        (*|> Binsec.Formula_transformation.remove_arrays*)
                        |> Binsec.Formula_pp.pp_formula Format.str_formatter
                    ;
                    let file = File.create "interval_pmc" "smt2" in
                    File.write (Format.flush_str_formatter ()) file;
                    let count = List.hd @@ Pmc.PMCount.Solver.query ~solver:!pmc_solver ~projections:([StateFormula.Projections.Projection.pp proj]) file in
                    match count.Result.result with
                    | Pmc.PMCount.CountVal.Count(z) -> Some(z)
                    | _ -> None
                in
                let mk_sure c =
                    let lo = c in
                    let hi = Z.add c Z.one in
                    Interval.create ~lo ~hi
                in
                let mk_weak c =
                    let lo = Z.of_int 2 in
                    let hi = Z.add c Z.one in
                    Interval.create ~lo ~hi
                in
                let mk_idk c =
                    let lo = Z.zero in
                    let hi = Z.add c Z.one in
                    Interval.create ~lo ~hi
                in
                match i with 
                | MaybeStrong(itv)
                | NotStrong(itv) ->
                (
                    if !pmc_solver = ""
                    then mk_weak @@ do_card itv
                    else
                    (
                        match do_pmc itv with
                        | Some(z) -> mk_sure @@ z
                        | _ -> mk_weak @@ do_card itv
                    )
                )
                | Unknown(itv) ->
                (
                    if !pmc_solver = ""
                    then mk_idk @@ do_card itv
                    else
                    (
                        match do_pmc itv with
                        | Some(z) -> mk_sure @@ z
                        | _ -> mk_idk @@ do_card itv
                    )
                )
                | Strong(itv) -> mk_sure @@ do_card itv 
            in
            let card itv = lazy (card itv) in
            create_aux constrprop constrres proj_size card itvs

        let cut_aux cut_itv res =
            let inside, outside = List.fold_left
                (
                    fun (inside, outside) (itv, _) ->
                        let maybe_add orig l = function
                            | Some(itv) -> (of_itv_kind itv orig)::l
                            | _ -> l
                        in
                        let itv_ = get_itv itv in
                        if Interval.overlap cut_itv itv_
                        then
                        (
                            let itvin = Some(Interval.inter itv_ cut_itv) in
                            let itvout1, itvout2 = Interval.sub itv_ cut_itv in
                            (maybe_add itv inside itvin),
                            (maybe_add itv (maybe_add itv outside itvout1) itvout2)
                        )
                        else inside, itv::outside
                )
                ([], []) res.itvs
            in
            inside, outside

        let cut cut_itv res =
            let inside, outside = cut_aux cut_itv res in
            create_aux res.constrprop res.constrres res.size res.card inside, 
            create_aux res.constrprop res.constrres res.size res.card outside

        let intersect a b =
            try
                if not (a.size = b.size) then raise (Failure("Size mismatch"));
                if not (a.constrprop = b.constrprop) then raise (Failure("Regularity constraint mismatch"));
                let nconstr =
                    match a.constrres, b.constrres with
                    | Some(constra), Some(constrb) ->
                    (
                        let intersect_constrs = Constraints.get_intersect a.constrprop in
                        Some(intersect_constrs constra constrb)
                    )
                    | None, None -> None
                    | _ -> raise (Failure("Regularity constraint result mismatch"))
                in
                let itvs = List.fold_left 
                    (
                        fun res (itv, _) -> 
                            let inside, _ = cut_aux (get_itv itv) b in
                            res @ List.map
                                (
                                    fun e ->
                                        match itv, e with
                                        | Strong(_), _
                                        | _, Strong(_) -> Strong(get_itv(e))
                                        | MaybeStrong(_), _
                                        | _, MaybeStrong(_) -> MaybeStrong(get_itv(e))
                                        | NotStrong(_), _
                                        | _, NotStrong(_) -> NotStrong(get_itv(e))
                                        | Unknown(_), Unknown(_) -> Unknown(get_itv(e))
                                )
                                inside
                    )
                    [] a.itvs
                in
                match nconstr, itvs with
                | Some(None), _
                | _, [] -> None
                | Some(nconstr), _
                | (None as nconstr), _ ->  Some(create_no_pmc a.constrprop nconstr a.size itvs)
            with e -> raise (Failure(Printf.sprintf "Could not intersect domains (%s)" @@ Printexc.to_string e))

        let constr_builder ?(filter = fun itv -> [itv]) domain v args =
            match v with
            | Binsec.Formula.BvVar(bv) ->
            (
                let regconstr =
                    let mk_constr =
                        match domain.constrres with
                        | Some(constrres) ->
                        (
                            let constr_builder = Constraints.get_constr_builder domain.constrprop in
                            match constr_builder constrres with
                            | Some(f) -> f
                            | _ -> fun _ _ -> Binsec.Formula.mk_bl_true
                        )
                        | _ -> fun _ _ -> Binsec.Formula.mk_bl_true
                    in
                    mk_constr v args
                in
                let bv = Binsec.Formula.mk_bv_fun bv args in
                Binsec.Formula.mk_bl_and regconstr
                    @@ List.fold_left
                        (
                            fun res itv ->
                                let itvconstr = StateFormula.FormulaUtils.interval (get_itv itv) bv in
                                Binsec.Formula.mk_bl_bnop Binsec.Formula.BlOr res itvconstr
                        )
                        Binsec.Formula.mk_bl_false
                        @@ List.fold_left (fun res (itv, _) -> (filter itv) @ res) [] domain.itvs
            )
            | _ -> Binsec.Formula.mk_bl_true 

        let of_generic = function
            | Result.Res(l) ->
            (
                let itvexp = Str.regexp {|I[0-9]+|} in
                let aux (constrprop, constr, size, itvs, min, maybe_min, max) r =
                    match r.Result.label, r.Result.result with
                    | "Count", Result.Res
                        (
                            [
                                {Result.result = Int(min); _};
                                {Result.result = Int(maybe_min); _};
                                {Result.result = Int(max); _};
                                _
                            ]
                        )
                            -> constrprop, constr, size, itvs, min, maybe_min, max
                    | "Regularity constraint", Result.Str(constrprop) -> constrprop, constr, size, itvs, min, maybe_min, max
                    | "Bitsize", Result.Int(size) -> constrprop, constr, size, itvs, min, maybe_min, max
                    | s, Result.Res 
                        (
                            [
                                {Result.result = Itv(itv, card_lo, card_hi); _};
                                {Result.result = Str(status); _}
                            ]
                        )
                            when Str.string_match itvexp s 0 ->
                    (
                        let itv =
                            match status with 
                            | "strong" -> Strong(itv)
                            | "maybe strong" -> MaybeStrong(itv)
                            | "weak" -> NotStrong(itv)
                            | "unknown" -> Unknown(itv)
                            | _ -> assert false
                        in
                        (*let carditv = Interval.create ~lo:cardlo ~hi:(Z.add card_hi Z.one) in*)
                        constrprop, constr, size, itv::itvs, min, maybe_min, max
                    )
                    | s, _ when s = constrprop -> 
                    (
                        let parsr = Constraints.get_parser s in
                        constrprop, parsr r, size, itvs, min, maybe_min, max
                    )
                    | _ -> assert false
                in
                let constrprop, constr, size, itvs, _, _, _ = List.fold_left aux ("None", None, Z.zero, [], Z.zero, Z.zero, Z.zero) l in
                create_no_pmc constrprop constr (Z.to_int size) itvs
            )
            | _ -> assert false

        let to_generic res =
            let constrpropres = Result.mk_string ~label:"Regularity constraint" res.constrprop in
            let constrlist = 
                match res.constrres with 
                | Some(Property.OK(Result.Custom(_, lazy res))) -> [constrpropres; res]
                | _ -> [constrpropres]
            in
            let sizeres = Result.mk_int ~label:"Bitsize" @@ Z.of_int res.size in
            let cnt = ref 0 in
            let itvlist = List.map
                (
                    fun (itv, card) ->
                    (
                        let card = Lazy.force card in
                        let label = Printf.sprintf "I%d" !cnt in
                        cnt := !cnt + 1;
                        let itv, status =
                            match itv with
                            | Strong(itv) -> itv, "strong"
                            | MaybeStrong(itv) -> itv, "maybe strong"
                            | NotStrong(itv) -> itv, "weak"
                            | Unknown(itv) -> itv, "unknown"
                        in
                        let itv = Result.mk_interval ~label:"interval" ~card:(Some(card.Interval.lo, Z.sub card.Interval.hi Z.one)) itv in
                        let status = Result.mk_string ~label:"status" status in
                        Result.mk_res_list ~label [itv; status]
                    )
                ) 
                res.itvs
            in
            let min = Result.mk_int ~label:"min" @@ Lazy.force res.min in
            let maybe_min = Result.mk_int ~label:"maybe min" @@ Lazy.force res.maybe_min in
            let max = Result.mk_int ~label:"max" @@ Lazy.force res.max in
            let fallback_count =
                if !pmc_solver = ""
                then Result.mk_bool ~label:"pmc fallback" false
                else Result.mk_string ~label:"pmc fallback" !pmc_solver
            in
            let count = Result.mk_res_list ~label:"Count" [min; maybe_min; max; fallback_count] in
            Result.Res(constrlist @ [sizeres] @ itvlist @ [count])
    end

module Res = Result.Make (ResVal)

let domains_from_generic_results res =
    Res.create ~label:res.Result.label @@ ResVal.of_generic res.result

let rec do_stitches l =
    match l with
    | (Strong(i))::(Strong(j))::t ->
    (
        try
            let l = (Strong(Interval.union i j))::t in
            do_stitches l
        with Failure _ -> (Strong(i))::(do_stitches ((Strong(j))::t))
    )
    | (MaybeStrong(i))::(MaybeStrong(j))::t ->
    (
        try
            let l = (MaybeStrong(Interval.union i j))::t in
            do_stitches l
        with Failure _ -> (MaybeStrong(i))::(do_stitches ((MaybeStrong(j))::t))
    )
    | (NotStrong(i))::(NotStrong(j))::t ->
    (
        try
            let l = (NotStrong(Interval.union i j))::t in
            do_stitches l
        with Failure _ -> (NotStrong(i))::(do_stitches ((NotStrong(j))::t))
    )
    | (Unknown(i))::(Unknown(j))::t ->
    (
        try
            let l = (Unknown(Interval.union i j))::t in
            do_stitches l
        with Failure _ -> (Unknown(i))::(do_stitches ((Unknown(j))::t))
    )
    | [] -> []
    | h::t -> h::(do_stitches t)

type exn += BoundNotFound

(*default max splits: maximum allowed*)
let sns 
    ?(split_max = match List.sort (fun a b -> compare b a) !split_max with h::_ -> h | [] -> (-1))
    ?(timeout = !timeout) 
    ?(constrprop = if !Constraints.default = [] then "None" else List.hd !Constraints.default) 
    ?(force_binsearch_bounds = !force_binsearch_bounds)
        proj sf =
    let constr = Constraints.get constrprop in
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

    let splits = new Stats.counter "splits" "Splits" 0 in
    let max_splits = new Stats.counter "max splits" "Maximum Splits" split_max in
    let timer = new Stats.timer "runtime" "Runtime" in
    let timeout_flag = new Stats.flag "timeout" "Timeout" in
    let stats = Stats.Bundle.create ""
        |> Stats.Bundle.add_stat (splits :> Stats.stat)
        |> Stats.Bundle.add_stat (max_splits :> Stats.stat)
        |> Stats.Bundle.add_stat (timer :> Stats.stat)
        |> Stats.Bundle.add_stat (timeout_flag :> Stats.stat)
    in

    let sf, constrres = constr proj sf in

    let rec aux stop todo res =
        match todo with
        | (h, hhalf)::t -> 
        (
            try
                Message.Wrap.send (Message.Wrap.Debug("S&S", lazy (Interval.pp h)));

                if (not stop) && timeout > 0 && timer#get > (Float.of_int timeout)
                then
                (
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy "S&S: timeout"));
                    timeout_flag#set;
                    aux true todo res
                )
                else
                (
                    (*interval constraint*)
                    let mk_constr i v args =
                        match v with
                        | Binsec.Formula.BvVar(v) ->
                        (
                            let bv = Binsec.Formula.mk_bv_fun v args in
                            StateFormula.FormulaUtils.interval i bv
                        )
                        | _ -> Binsec.Formula.mk_bl_true
                    in
                    let sf_ = StateFormula.add_proj_constr proj (mk_constr h) sf in

                    (*shrinking*)
                    let lo =
                        match hhalf with
                        | NA
                        | Right ->
                        (
                            if force_binsearch_bounds
                            then 
                            (
                                try
                                    Newsome.bin_search_lower_bound proj h sf
                                with Not_found -> raise BoundNotFound
                            )
                            else
                            (
                                match Bounds.check Bounds.Minimize proj sf_ with
                                | Property.OK(Result.Custom((Bounds.Res.CustomResult(bnd)), _)) -> bnd.Result.result
                                | _ -> 
                                (
                                    Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "S&S: resorting to binary search to find lower bound in %s" @@ Interval.pp h)));
                                    try
                                        Newsome.bin_search_lower_bound proj h sf
                                    with Not_found -> raise BoundNotFound
                                )
                            )
                        )
                        | _ -> h.lo
                    in
                    let hi =
                        match hhalf with
                        | NA
                        | Left ->
                        (
                            if force_binsearch_bounds
                            then
                            (
                                try
                                    Z.add Z.one @@ Newsome.bin_search_upper_bound proj h sf
                                with Not_found -> raise BoundNotFound
                            )
                            else
                            (
                                match Bounds.check Bounds.Maximize proj sf_ with
                                | Property.OK(Result.Custom((Bounds.Res.CustomResult(bnd)), _)) -> Z.add bnd.Result.result Z.one
                                | _ -> 
                                (
                                    Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "S&S: resorting to binary search to find upper bound in %s" @@ Interval.pp h)));
                                    try
                                        Z.add Z.one @@ Newsome.bin_search_upper_bound proj h sf
                                    with Not_found -> raise BoundNotFound
                                )
                            )
                        )
                        | _ -> h.hi
                    in
                    let i = Interval.create ~lo ~hi in
                    Message.Wrap.send (Message.Wrap.Debug("S&S", lazy (Printf.sprintf "%s > %s" (Interval.pp h) @@ Interval.pp i)));
                    if Interval.card i < Z.of_int 3
                    then aux stop t ((Strong(i))::res)
                    else
                    (
                        let sf = StateFormula.add_proj_constr proj (mk_constr i) sf in

                        (*splitting*)
                        match StrongControl.check proj sf with
                        | Property.OK(Result.Custom((StrongControl.Res.CustomResult({Result.result = StrongControl.ResVal.NotStrong(Some(pivot)); _})), _)) ->
                        (
                            if splits#get = split_max || stop
                            then aux stop t ((NotStrong(i))::res)
                            else
                            (
                                if Interval.card i = Z.one
                                then aux stop t res
                                else
                                (
                                    try
                                        Message.Wrap.send (Message.Wrap.Debug("S&S", lazy (Printf.sprintf "Splitting around %s" @@ Z.format "%#x" pivot)));
                                        let l = Interval.split pivot i in
                                        splits#incr;
                                        if splits#get = split_max
                                        then Message.Wrap.send (Message.Wrap.BigWarning(lazy "S&S: max splits reached"));
                                        aux stop (t @ (mark_halves l)) res
                                    with Failure e -> 
                                    (
                                        Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "S&S: split failed (%s)" e)));
                                        aux stop t ((NotStrong(i))::res)
                                    )
                                )
                            )
                        )
                        | OK(Custom((StrongControl.Res.CustomResult({result = NotStrong(None); _})), _)) ->
                        (
                            Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "S&S: could not get model for pivot in %s" @@ Interval.pp i)));
                            aux stop t ((NotStrong(i))::res)
                        )
                        | OK(Custom((StrongControl.Res.CustomResult({result = Strong; _})), _)) -> aux stop t ((Strong(i))::res)
                        | _ -> 
                        (
                            Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "S&S: error when checking strong control in %s" @@ Interval.pp i)));
                            aux stop t ((MaybeStrong(i))::res)
                        )
                    )
                )
            with BoundNotFound ->
            (
                Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printf.sprintf "S&S: could not find bound in %s" @@ Interval.pp h)));
                aux stop t ((Unknown(h))::res)
            )
        )
        | _ -> res
    in

    let lo = Z.zero in
    let hi = Z.pow (Z.of_int 2) proj_size in
    let imax = Interval.create ~lo ~hi in
    [(imax, NA)]
        |> fun e -> 
            (
                timer#start;
                let res = aux false e [] in
                timer#stop;
                res
            )
        |> List.sort 
            (
                fun i j ->
                (
                    let i = get_itv i in
                    let j = get_itv j in
                    Interval.compare i j
                )
            )
        |> do_stitches
        |> ResVal.create proj proj_def proj_var constrprop constrres sf
        |> Res.create ~label:"S&S" ~stats:(Some(stats))

module ListResVal =
    struct
        type t = (string * (int * Res.result) list) list

        let to_generic l =
            Result.Res
                (
                    List.map 
                        (
                            fun (label, l) ->
                                Result.mk_res_list ~label
                                    @@ List.map
                                        (
                                            fun (splits, r) -> 
                                                let label =
                                                    if splits < 0
                                                    then "no split limit"
                                                    else Printf.sprintf "%d max splits" splits
                                                in
                                                {(Res.export r) with Result.label}
                                        )
                                        l
                        ) 
                        l
                )
    end

module ListRes = Result.Make(ListResVal)

let check proj sf =
    let res = List.map 
        ( 
            fun constrprop ->
                let suff = Constraints.get_suff constrprop in
                "S&S" ^ suff, List.map (fun split_max -> split_max, sns ~split_max ~constrprop proj sf) !split_max
        )
        !Constraints.default
    in
    Property.mk_ok "S&S" proj None @@ ListRes.wrap @@ ListRes.create ~label:"S&S" res

module Bytewise =
    struct
        module ResVal =
            struct
                type t = Res.result list

                let create proj sf =
                    let proj_def =
                        try
                            StateFormula.Projections.find proj @@ StateFormula.get_projections sf
                        with Not_found -> assert false
                    in
                    let proj_var, _ = StateFormula.FormulaUtils.def_var_args proj_def in
                    let proj_size =
                        match proj_var with
                        | Binsec.Formula.BvVar(bv) -> bv.Binsec.Formula.bv_size
                        | _ -> assert false
                    in
                    List.init (proj_size / 8)
                        (
                            fun byte ->
                                let proj, sf = StateFormula.add_proj_restrict ~lo:byte ~hi:byte proj sf in
                                let domain = sns ~constrprop:"None" proj sf in
                                {domain with Result.label = Printf.sprintf "byte %d" byte}
                        )

                let mk_constr_builder domains =
                    fun v args ->
                        match v with
                        | Binsec.Formula.BvVar(v) ->
                        (
                            let aux (byte, res) domain =
                                let bv = Binsec.Formula.mk_bv_fun v args in
                                let extract = Binsec.Formula.mk_bv_extract {Binsec.Interval.lo = byte * 8; Binsec.Interval.hi = byte * 8 + 7} bv in
                                byte + 1,
                                Binsec.Formula.mk_bl_and res
                                    @@ List.fold_left
                                        (
                                            fun res (itv, _) ->
                                                let itvconstr = StateFormula.FormulaUtils.interval (get_itv itv) extract in
                                                Binsec.Formula.mk_bl_bnop Binsec.Formula.BlOr res itvconstr
                                        )
                                        Binsec.Formula.mk_bl_false domain.Result.result.ResVal.itvs
                            in
                            let _, constr = List.fold_left aux (0, Binsec.Formula.mk_bl_true) domains in
                            constr
                        )
                        | _ -> Binsec.Formula.mk_bl_true

                let mk_constr proj sf domains =
                    let mk_byte_constr (byte, sf) domain =
                        let byte_constr_builder v args = 
                            match v with
                            | Binsec.Formula.BvVar(v) ->
                            (
                                let bv = Binsec.Formula.mk_bv_fun v args in
                                let extract = Binsec.Formula.mk_bv_extract {Binsec.Interval.lo = byte * 8; Binsec.Interval.hi = byte * 8 + 7} bv in
                                List.fold_left
                                    (
                                        fun res (itv, _) ->
                                            let itvconstr = StateFormula.FormulaUtils.interval (get_itv itv) extract in
                                            Binsec.Formula.mk_bl_bnop Binsec.Formula.BlOr res itvconstr
                                    )
                                    Binsec.Formula.mk_bl_false domain.Result.result.ResVal.itvs
                            )
                            | _ -> Binsec.Formula.mk_bl_true
                        in
                        byte + 1, StateFormula.add_proj_constr proj byte_constr_builder sf
                    in
                    let _, sf = List.fold_left mk_byte_constr (0, sf) domains in
                    sf

                let intersect a b =
                    try
                        Some
                            (
                                List.map2 
                                    (
                                        fun a b -> 
                                            match ResVal.intersect a.Result.result b.Result.result with
                                            | Some(inter) -> Res.create ~label:a.Result.label inter
                                            | None -> raise (Failure(""))
                                    ) 
                                    a b
                            )
                    with _ -> None

                let to_generic r =
                    Result.Res(List.map Res.export r)

                let of_generic = function
                    | Result.Res(l) -> List.map domains_from_generic_results l
                    | _ -> assert false
            end

        module Res = Result.Make(ResVal)

        let bytedoms_from_generic_results res =
            Res.create ~label:res.Result.label @@ ResVal.of_generic res.result

        let _ =
            let mk_constr proj sf =
                let byte_domains = ResVal.create proj sf in
                ResVal.mk_constr proj sf byte_domains, Some(Property.mk_ok "ByteDomains" proj None @@ Res.wrap @@ Res.create ~label:"ByteDomains" byte_domains)
            in
            let mk_constr_builder = function
                | Property.OK(Result.Custom((Res.CustomResult(bytedoms)), _)) -> Some(ResVal.mk_constr_builder bytedoms.Result.result)
                | _ -> None
            in
            let parsr res =
                Some(Property.OK(Res.wrap @@ bytedoms_from_generic_results res))
            in
            let intersect a b =
                match a, b with
                | Property.OK(Result.Custom(Res.CustomResult(a), _)), Property.OK(Result.Custom(Res.CustomResult(b), _)) ->
                (
                    match ResVal.intersect a.Result.result b.Result.result with
                    | Some(inter) -> Some(Property.OK(Res.wrap @@ Res.create ~label:"ByteDomains" inter))
                    | _ -> None
                )
                | _ -> assert false
            in
            Constraints.register "ByteDomains" "Account for individual byte domains." "BD" mk_constr mk_constr_builder parsr intersect
    end

let _ =
    Options.register_option ("-sns-l", Arg.String parse_splits, "Maximum number of splits allowed during shrink and split.");
    Options.register_option ("-sns-t", Arg.Set_int timeout, "Set timeout (s).");
    Options.register_option ("-sns-pmc", Arg.Set_string pmc_solver, "Set PMC solver to count models in non-strong intervals.");
    Options.register_option ("-sns-force-binsearch-bounds", Arg.Set force_binsearch_bounds, "Force binary search fallback when looking for feasible bounds in intervals. Useful if Z3 is performing poorly.");
    Property.register "S&S" "compute domains of control with Shrink and Split" check None
