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

let stats = StateFormula.SolverStats.create ~report:false "Fixed Bits Property"

module MaskVal =
    struct
        type t =
            {
                mask : Z.t;
                bits : Z.t
            }

        let to_generic res = 
            Result.Res([Result.mk_int ~label:"mask" res.mask; Result.mk_int ~label:"bits" res.bits])

        let of_generic = function
            | Result.Res([{Result.result = Int(mask); _}; {Result.result = Int(bits); _}]) -> {mask; bits}
            | _ -> assert false

        let mk_constr_builder mv =
            if mv.mask = Z.zero
            then None
            else
            (
                let builder v args =
                    match v with
                    | Binsec.Formula.BvVar(v) ->
                    (
                        let bv = Binsec.Formula.mk_bv_fun v args in
                        let mask = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create mv.mask v.Binsec.Formula.bv_size in
                        let bits = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create mv.bits v.Binsec.Formula.bv_size in
                        (*let zero = Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create Z.zero proj_size in*)
                        let bv_and = Binsec.Formula.mk_bv_and bv mask in
                        Binsec.Formula.mk_bv_equal bv_and bits
                    )
                    | _ -> Binsec.Formula.mk_bl_true
                in
                Some(builder)
            )

        let mk_constr proj sf mv =
            try
                match mk_constr_builder mv with
                | Some(builder) -> StateFormula.add_proj_constr proj builder sf
                | None -> sf
            with Not_found -> sf

        let intersect a b =
            let open Z in
            let common = a.mask land b.mask in
            if (common land a.bits) = (common land b.bits)
            then 
            (
                let mask = a.mask lor b.mask in
                let bits = a.bits lor b.bits in
                Some({mask; bits})
            )
            else None
    end

module Res = Result.Make(MaskVal)

let mv_of_generic_result res = 
    Res.create ~label:res.Result.label @@ MaskVal.of_generic res.result

let check proj sf =
    let sf, vals =
        let free = StateFormula.get_free_variables sf in
        let pc = StateFormula.get_path_constraint sf in
        let projdef =
            try
                StateFormula.Projections.find proj @@ StateFormula.get_projections sf
            with Not_found -> assert false
        in
        let proj_var, _ = StateFormula.FormulaUtils.def_var_args projdef in

        let x = List.map (StateFormula.FormulaUtils.clone_var ~name:"x") free in
        let x_decl = List.map (fun e -> StateFormula.FormulaUtils.var_to_decl e []) x in
        let x_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) x_decl in

        let constrx = StateFormula.FormulaUtils.get_bl @@ StateFormula.FormulaUtils.def_to_term pc x_term in
        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term projdef x_term in

        let y = List.map (StateFormula.FormulaUtils.clone_var ~name:"y") free in
        let y_decl = List.map (fun e -> StateFormula.FormulaUtils.var_to_decl e []) y in
        let y_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) y_decl in

        let constry = StateFormula.FormulaUtils.get_bl @@ StateFormula.FormulaUtils.def_to_term pc y_term in
        let projy = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term projdef y_term in

        let mask = StateFormula.FormulaUtils.clone_var ~name:"mask" proj_var in
        let mask_decl = StateFormula.FormulaUtils.var_to_decl mask [] in
        let mask_term = StateFormula.FormulaUtils.decl_to_term mask_decl [] in
        let mask_term_bv = StateFormula.FormulaUtils.get_bv mask_term in

        let bits = StateFormula.FormulaUtils.clone_var ~name:"bits" proj_var in
        let bits_decl = StateFormula.FormulaUtils.var_to_decl bits [] in
        let bits_term = StateFormula.FormulaUtils.decl_to_term bits_decl [] in
        let bits_term_bv = StateFormula.FormulaUtils.get_bv bits_term in

        let mask_constr = Binsec.Formula.mk_bv_equal mask_term_bv
            @@ Binsec.Formula.mk_bv_bnop Binsec.Formula.BvXnor projx projy
        in

        let bits_constr = Binsec.Formula.mk_bv_equal bits_term_bv
            @@ Binsec.Formula.mk_bv_bnop Binsec.Formula.BvAnd projx projy
        in

        let test = List.map (StateFormula.FormulaUtils.clone_var ~name:"test") free in
        let test_decl = List.map (fun e -> StateFormula.FormulaUtils.var_to_decl e []) test in
        let test_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) test_decl in

        let constrtest = StateFormula.FormulaUtils.get_bl @@ StateFormula.FormulaUtils.def_to_term pc test_term in
        let projtest = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term projdef test_term in

        let add_test sf =
            if test = []
            then sf
            else
            (
                let test = StateFormula.FormulaUtils.Quant.mk_quant_notexists test_decl
                    @@ Binsec.Formula.mk_bl_and constrtest
                    @@ Binsec.Formula.mk_bv_distinct bits_term_bv
                    @@ Binsec.Formula.mk_bv_and projtest mask_term_bv
                in
                StateFormula.add_custom (StateFormula.FormulaUtils.Quant.to_entry test) sf
            )
        in

        (
            sf
                |> List.fold_right StateFormula.add_declare x_decl
                |> List.fold_right StateFormula.add_declare y_decl
                |> StateFormula.add_declare mask_decl
                |> StateFormula.add_declare bits_decl
                |> StateFormula.add_assert constrx
                |> StateFormula.add_assert constry
                |> StateFormula.add_assert mask_constr
                |> StateFormula.add_assert bits_constr
                |> add_test
        ),
        [mask_term_bv; bits_term_bv]
    in

    stats.timer#start;
    let status = StateFormula.check_sat ~get:vals sf in
    stats.timer#stop;
    Property.incr stats status;
    match status.Result.result with
    | StateFormula.StatusVal.Sat([(_, mask); (_, bits)]) -> 
    (
        let maskval = {MaskVal.mask; bits} in
        Property.mk_ok "FixedBits" proj (Some(sf)) @@ Res.wrap @@ Res.create ~label:"FixedBits" ~stats:status.stats maskval
    )
    | _ -> Property.mk_ko "FixedBits" proj (Some(sf)) @@ StateFormula.Status.wrap status

let mk_constr proj sf =
    let res = check proj sf in
    match res with
    | Property.OK(Result.Custom((Res.CustomResult(mv)), _)) -> MaskVal.mk_constr proj sf mv.Result.result, Some(res)
    | _ -> sf, None

let mk_constr_builder = function
    | Property.OK(Result.Custom((Res.CustomResult(mv)), _)) -> MaskVal.mk_constr_builder mv.Result.result
    | _ -> None

let parsr res =
    Some(Property.OK(Res.wrap @@ mv_of_generic_result res))

let intersect a b =
    match a, b with
    | Property.OK(Result.Custom(Res.CustomResult(a), _)), Property.OK(Result.Custom(Res.CustomResult(b), _)) ->
    (
        match MaskVal.intersect a.Result.result b.Result.result with
        | Some(inter) -> Some(Property.OK(Res.wrap @@ Res.create ~label:"FixedBits" inter))
        | None -> None
    )
    | _ -> assert false

let _ = 
    Property.register "FixedBits" "fixed bits of projection (single query)" check (Some(stats));
    Sns.Constraints.register "FixedBits" "Account for fixed bits." "FB" mk_constr mk_constr_builder parsr intersect
