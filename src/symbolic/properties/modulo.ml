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

let stats = StateFormula.SolverStats.create ~report:false "Modulo Property"

module ModVal =
    struct
        type t =
            {
                div : Z.t;
                rem : Z.t
            }

        let to_generic r =
            Result.Res([Result.mk_int ~label:"divisor" r.div; Result.mk_int ~label:"remainder" r.rem])
    end

module Res = Result.Make(ModVal)

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

        let d = StateFormula.FormulaUtils.clone_var ~name:"d" proj_var in
        let d_decl = StateFormula.FormulaUtils.var_to_decl d [] in
        let d_term = StateFormula.FormulaUtils.decl_to_term d_decl [] in
        let d_term_bv = StateFormula.FormulaUtils.get_bv d_term in

        let r = StateFormula.FormulaUtils.clone_var ~name:"r" proj_var in
        let r_decl = StateFormula.FormulaUtils.var_to_decl r [] in
        let r_term = StateFormula.FormulaUtils.decl_to_term r_decl [] in
        let r_term_bv = StateFormula.FormulaUtils.get_bv r_term in

        let constrx = StateFormula.FormulaUtils.get_bl @@ StateFormula.FormulaUtils.def_to_term pc x_term in
        let projx = StateFormula.FormulaUtils.get_bv @@ StateFormula.FormulaUtils.def_to_term projdef x_term in
        let eq = Binsec.Formula.mk_bv_equal r_term_bv
            @@ Binsec.Formula.mk_bv_bnop Binsec.Formula.BvUrem projx d_term_bv 
        in
        let impl = Binsec.Formula.mk_bl_bnop Binsec.Formula.BlImply constrx eq in
        let forall = StateFormula.FormulaUtils.Quant.to_entry @@ StateFormula.FormulaUtils.Quant.mk_quant_forall x_decl impl in

        let constrd = Binsec.Formula.mk_bv_comp Binsec.Formula.BvUlt (Binsec.Formula.mk_bv_cst @@ Binsec.Bitvector.create Z.one d_term_bv.Binsec.Formula.bv_term_size) d_term_bv in

        (
            sf
                |> StateFormula.add_declare d_decl
                |> StateFormula.add_declare r_decl
                |> StateFormula.add_assert constrd
                |> StateFormula.add_custom forall
                |> StateFormula.set_quant true
        ),
        [d_term_bv; r_term_bv]
    in

    stats.timer#start;
    let status = StateFormula.check_sat ~get:vals sf in
    stats.timer#stop;
    Property.incr stats status;
    let label = "Modulo" in
    let stats = status.Result.stats in
    match status.Result.result with
    | StateFormula.StatusVal.Sat([(_, div); (_, rem)]) ->
    (
        let modval = {ModVal.div; rem} in
        Property.mk_ok "Modulo" proj (Some(sf)) @@ Res.wrap @@ Res.create ~label ~stats modval
    )
    | _ -> Property.mk_ko "Modulo" proj (Some(sf)) @@ StateFormula.Status.wrap status

let _ =
    Property.register "Modulo" "find common modulo and remainder" check (Some(stats))
