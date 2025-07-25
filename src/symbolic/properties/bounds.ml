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

type command = Minimize | Maximize
let statsl = StateFormula.SolverStats.create ~report:false "Lower Bound Property"
let statsu = StateFormula.SolverStats.create ~report:false "Upper Bound Property"

let mk_cmd = function
    | Minimize -> StateFormula.FormulaUtils.CustomCmd.Minimize.mk_cmd 
    | Maximize -> StateFormula.FormulaUtils.CustomCmd.Maximize.mk_cmd 

let prop_name cmd =
    match cmd with
    | Minimize -> "LowerBound"
    | Maximize -> "UpperBound"

module ResVal =
    struct
        type t = Z.t

        let to_generic v =
            Result.Int(v)
    end

module Res = Result.Make(ResVal)

let check cmd proj sf =
    (*let sf =
        if StateFormula.has_free_array sf
        then 
        (
            let projections = StateFormula.get_projections sf in
            StateFormula.create ~projections @@ Binsec.Formula_transformation.remove_arrays @@ StateFormula.get_base sf
        )
        else sf
    in*)
    let sf, proj_term =
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

        let y = StateFormula.FormulaUtils.clone_var ~name:"y" proj_var in
        let y_decl = StateFormula.FormulaUtils.var_to_decl y [] in
        let y_term = StateFormula.FormulaUtils.decl_to_term y_decl [] in
        let y_term_bv = StateFormula.FormulaUtils.get_bv y_term in

        let constrx = StateFormula.FormulaUtils.def_to_term pc x_term in
        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term projdef x_term in
        let eq = Binsec.Formula.mk_bv_equal projx y_term_bv in
        (
            sf
                |> List.fold_right StateFormula.add_declare x_decl
                |> StateFormula.add_declare y_decl
                |> StateFormula.add_assert (StateFormula.FormulaUtils.get_bl constrx)
                |> StateFormula.add_assert eq
                |> StateFormula.add_custom ((mk_cmd cmd) y_term)
                |> StateFormula.set_solver (Some(StateFormula.MultiSolver.Z3))
        ),
        y_term_bv
    in
    let stats =
        match cmd with
        | Minimize -> statsl
        | Maximize -> statsu
    in
    stats.timer#start;
    let status = StateFormula.check_sat ~get:[proj_term] sf in
    stats.timer#stop;
    Property.incr stats status;
    let name = prop_name cmd in
    match status.Result.result with
    | StateFormula.StatusVal.Sat([(_, z)]) -> Property.mk_ok (prop_name cmd) proj (Some(sf)) @@ Res.wrap @@ Res.create ~label:name ~stats:status.stats z
    | _ -> Property.mk_ko (prop_name cmd) proj (Some(sf)) @@ StateFormula.Status.wrap status

let _ =
    Property.register "LowerBound" "lower bound of projection" (check Minimize) (Some(statsl)); 
    Property.register "UpperBound" "upper bound of projection" (check Maximize) (Some(statsu))

