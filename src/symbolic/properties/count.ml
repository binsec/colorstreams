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

module ResVal =
    struct
        type t = Pmc.PMCount.Count.result list

        let to_generic r =
            Result.Res(List.map Pmc.PMCount.Count.export r)
    end

module Res = Result.Make(ResVal)

let check proj sf =
    let fname = (StateFormula.Projections.Projection.pp proj) ^ "_Count" in
    let file = File.create ~keep:true fname ".smt2" in
    sf
        |> StateFormula.get_base
        (*|> Binsec.Formula_transformation.remove_arrays*)
        |> Binsec.Formula_pp.pp_formula Format.str_formatter
    ;
    let fml = Format.flush_str_formatter () in
    File.write fml file;
    let res = file
        |> Pmc.PMCount.Solver.query ~projections:([StateFormula.Projections.Projection.pp proj])
        |> Res.create ~label:"Projected Model Counting"
        |> Res.wrap
    in
    Property.mk_ok "Count" proj None res

let _ =
    Property.register "Count" "projected model counting" check None
