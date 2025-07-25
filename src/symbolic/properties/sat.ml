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

let stats = StateFormula.SolverStats.create ~report:false "Sat Property"

let check proj sf =
    let sf, var =
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

        let constrx = StateFormula.FormulaUtils.get_bl @@ StateFormula.FormulaUtils.def_to_term pc x_term in
        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term projdef x_term in
        let constry = Binsec.Formula.mk_bv_equal y_term_bv projx in
        sf
            |> List.fold_right StateFormula.add_declare x_decl
            |> StateFormula.add_declare y_decl
            |> StateFormula.add_assert constrx
            |> StateFormula.add_assert constry
        ,
        y_term_bv
    in

    stats.timer#start;
    let status = StateFormula.check_sat ~get:[var] sf in
    stats.timer#stop;
    Property.incr stats status;

    match status.Result.result with
    | StateFormula.StatusVal.Sat(_)
    | Unsat -> Property.mk_ok "Sat" proj (Some(sf)) @@ StateFormula.Status.wrap status
    | _ -> Property.mk_ko "Sat" proj (Some(sf)) @@ StateFormula.Status.wrap status

let _ =
    Property.register "Sat" "basic sat check" check (Some(stats))
