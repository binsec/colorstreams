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

let name = "WeakControl"
let desc = "there exists at least two different inputs yielding different values"
let stats = StateFormula.SolverStats.create ~report:false (name ^ " Property")

module ResVal =
    struct
        type t = Weak | NotWeak

        let to_generic = function
            | Weak -> Result.Bool(true)
            | NotWeak -> Result.Bool(false)
    end

module Res = Result.Make(ResVal)

let check proj sf =
    let sf =
        let free = StateFormula.get_free_variables sf in
        let pc = StateFormula.get_path_constraint sf in
        let proj_def = 
            try
                StateFormula.Projections.find proj @@ StateFormula.get_projections sf 
            with Not_found -> assert false
        in

        let x = List.map (StateFormula.FormulaUtils.clone_var ~name:"x") free in
        let x_decl = List.map (fun e -> StateFormula.FormulaUtils.var_to_decl e []) x in
        let x_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) x_decl in

        let y = List.map (StateFormula.FormulaUtils.clone_var ~name:"y") free in
        let y_decl = List.map (fun e -> StateFormula.FormulaUtils.var_to_decl e []) y in
        let y_term = List.map (fun e -> StateFormula.FormulaUtils.decl_to_term e []) y_decl in

        let constrx = StateFormula.FormulaUtils.def_to_term pc x_term in
        let constry = StateFormula.FormulaUtils.def_to_term pc y_term in

        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term proj_def x_term in
        let projy = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term proj_def y_term in
        let neq = Binsec.Formula.mk_bv_distinct projx projy in
        sf
            |> List.fold_right StateFormula.add_declare x_decl
            |> List.fold_right StateFormula.add_declare y_decl
            |> StateFormula.add_assert (StateFormula.FormulaUtils.get_bl constrx)
            |> StateFormula.add_assert (StateFormula.FormulaUtils.get_bl constry)
            |> StateFormula.add_assert neq
    in

    stats.timer#start;
    let status = StateFormula.check_sat sf in
    stats.timer#stop;
    Property.incr stats status;

    match status.Result.result with
    | StateFormula.StatusVal.Sat(_) -> Property.mk_ok "WeakControl" proj (Some(sf)) @@ Res.wrap @@ Res.create ~label:"WeakControl" ~stats:status.stats ResVal.Weak
    | Unsat -> Property.mk_ok "WeakControl" proj (Some(sf)) @@ Res.wrap @@ Res.create ~label:"WeakControl" ~stats:status.stats ResVal.NotWeak
    | Error -> Property.mk_ko "WeakControl" proj (Some(sf)) @@ StateFormula.Status.wrap status

let _ =
    Property.register name desc check (Some(stats))
