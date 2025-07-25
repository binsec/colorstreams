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

let statss = StateFormula.SolverStats.create ~report:false "Strong Control Property"

module ResVal =
    struct
        type t = Strong | NotStrong of Z.t option

        let to_generic = function
            | Strong -> Result.Bool(true)
            | NotStrong(Some(counter)) ->
            (
                let ansres = Result.mk_bool ~label:"result" false in
                let counterres = Result.mk_int ~label:"infeasible" counter in
                Result.Res([ansres; counterres])
            )
            | NotStrong(_) -> Result.Bool(false)
    end

module Res = Result.Make(ResVal)

let check proj sf =
    let sf, var =
        let free = StateFormula.get_free_variables sf in
        let pc = StateFormula.get_path_constraint sf in
        let proj_def = 
            try
                StateFormula.Projections.find proj @@ StateFormula.get_projections sf 
            with Not_found -> assert false
        in
        let proj_var, _ = StateFormula.FormulaUtils.def_var_args proj_def in

        let x = List.map (StateFormula.FormulaUtils.clone_var ~name:"x") free in
        let x_decl = List.map (fun e -> StateFormula.FormulaUtils.var_to_decl e []) x in
        let x_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) x_decl in

        let y = StateFormula.FormulaUtils.clone_var ~name:"y" proj_var in
        let y_decl = StateFormula.FormulaUtils.var_to_decl y [] in
        let y_term = StateFormula.FormulaUtils.decl_to_term y_decl [] in
        let y_term_bv = StateFormula.FormulaUtils.get_bv y_term in

        let constrx = StateFormula.FormulaUtils.get_bl @@ StateFormula.FormulaUtils.def_to_term pc x_term in
        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term proj_def x_term in
        let neq = Binsec.Formula.mk_bv_distinct projx y_term_bv in
        let impl = Binsec.Formula.mk_bl_bnop Binsec.Formula.BlImply constrx neq in

        let constry, sf =
            try
                let cb = StateFormula.get_proj_constr proj sf in
                cb y [],
                StateFormula.remove_proj_constr proj sf
            with Not_found -> Binsec.Formula.mk_bl_true, sf
        in

        let add_quant sf =
            if x = []
            then StateFormula.add_assert impl sf
            else StateFormula.add_custom (StateFormula.FormulaUtils.Quant.to_entry @@ StateFormula.FormulaUtils.Quant.mk_quant_forall x_decl impl) sf
        in

        (
            sf
                |> StateFormula.add_declare y_decl
                |> StateFormula.add_assert constry
                |> add_quant
                |> StateFormula.set_quant true
        ), 
        [y_term_bv]
    in

    let stats = statss in
    stats.timer#start;
    let status = StateFormula.check_sat ~get:var sf in
    stats.timer#stop;
    Property.incr stats status;

    let label = "StrongControl" in
    match status.Result.result with
    | StateFormula.StatusVal.Sat([(_, z)]) -> Property.mk_ok label proj (Some(sf)) @@ Res.wrap @@ Res.create ~label ~stats:status.stats (ResVal.NotStrong(Some(z)))
    | Sat(_) -> Property.mk_ok label proj (Some(sf)) @@ Res.wrap @@ Res.create ~label ~stats:status.stats (ResVal.NotStrong(None))
    | Unsat -> Property.mk_ok label proj (Some(sf)) @@ Res.wrap @@ Res.create ~label ~stats:status.stats ResVal.Strong
    | _ -> Property.mk_ko label proj (Some(sf)) @@ StateFormula.Status.wrap status

let _ =
    Property.register "StrongControl" "there exists inputs for every value" check (Some(statss));
    Property.register "NotStrongControl" "there exists a value which cannot be obtained" check (Some(statss))
