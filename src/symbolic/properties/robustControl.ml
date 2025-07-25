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

let name = "RobustControl"
let desc = "values are constant with regard to uncontrolled inputs (use the \"controlled\" source keyword to mark inputs as controlled)"
let stats = StateFormula.SolverStats.create ~report:false (name ^ " Property")

module ResVal =
    struct
        type t = Robust | NotRobust

        let to_generic = function
            | Robust -> Result.Bool(true)
            | NotRobust -> Result.Bool(false)
    end

module Res = Result.Make(ResVal)

let check proj sf =
    let sf =
        let free = StateFormula.get_free_variables sf in
        let inputs = StateFormula.get_inputs sf in
        let controlled = StateFormula.Inputs.fold 
            (
                fun input e res -> 
                    if List.mem "controlled" @@ StateFormula.Inputs.Input.get_desc input
                    then e::res
                    else res
            )
            inputs []
        in
        let uncontrolled = List.filter (fun e -> not @@ List.mem e controlled) @@ List.mapi (fun i e -> i, e) free in
        let pc = StateFormula.get_path_constraint sf in
        let proj_def =
            try
                StateFormula.Projections.find proj @@ StateFormula.get_projections sf
            with Not_found -> assert false
        in

        let a = List.map (fun (idx, e) -> idx, StateFormula.FormulaUtils.clone_var ~name:"a" e) controlled in
        let a_decl = List.map (fun (idx, e) -> idx, StateFormula.FormulaUtils.var_to_decl e []) a in

        let x = List.map (fun (idx, e) -> idx, StateFormula.FormulaUtils.clone_var ~name:"x" e) uncontrolled in
        let x_decl = List.map (fun (idx, e) -> idx, StateFormula.FormulaUtils.var_to_decl e []) x in

        let y = List.map (fun (idx, e) -> idx, StateFormula.FormulaUtils.clone_var ~name:"y" e) uncontrolled in
        let y_decl = List.map (fun (idx, e) -> idx, StateFormula.FormulaUtils.var_to_decl e []) y in

        let reorder a b =
            List.map (fun (_, e) -> e) @@ List.sort (fun (a, _) (b, _) -> compare a b) (a @ b)
        in

        let x_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) @@reorder a_decl x_decl in

        let y_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) @@ reorder a_decl y_decl in

        let constrx = StateFormula.FormulaUtils.def_to_term pc x_term in
        let constry = StateFormula.FormulaUtils.def_to_term pc y_term in

        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term proj_def x_term in
        let projy = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term proj_def y_term in

        let neq = Binsec.Formula.mk_bv_distinct projx projy in

        sf
            |> List.fold_right (fun (_, e) res -> StateFormula.add_declare e res) a_decl
            |> List.fold_right (fun (_, e) res -> StateFormula.add_declare e res) x_decl
            |> List.fold_right (fun (_, e) res -> StateFormula.add_declare e res) y_decl
            |> StateFormula.add_assert (StateFormula.FormulaUtils.get_bl constrx)
            |> StateFormula.add_assert (StateFormula.FormulaUtils.get_bl constry)
            |> StateFormula.add_assert neq
    in

    stats.timer#start;
    let status = StateFormula.check_sat sf in
    stats.timer#stop;
    Property.incr stats status;

    match status.Result.result with
    | StateFormula.StatusVal.Sat(_) -> Property.mk_ok name proj (Some(sf)) @@ Res.wrap @@ Res.create ~label:name ~stats:status.stats ResVal.NotRobust
    | Unsat -> Property.mk_ok name proj (Some(sf)) @@ Res.wrap @@ Res.create ~label:name ~stats:status.stats ResVal.Robust
    | Error -> Property.mk_ko name proj (Some(sf)) @@ StateFormula.Status.wrap status

let _ =
    Property.register name desc check (Some(stats))
