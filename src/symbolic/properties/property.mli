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

(**framework for checking properties on state formulas*)

type 'a ok = OK of 'a | KO of 'a | MsgKO of string * Stats.Bundle.t option
type propres = 
    {
        proj : StateFormula.Projections.Projection.t;
        result : Result.wrapped_result ok
    }

val mk_ok : string -> StateFormula.Projections.Projection.t -> StateFormula.t option -> Result.wrapped_result -> Result.wrapped_result ok
val mk_ko : string -> StateFormula.Projections.Projection.t -> StateFormula.t option -> Result.wrapped_result -> Result.wrapped_result ok
val mk_ko_msg : ?stats:(Stats.Bundle.t option) -> string -> StateFormula.Projections.Projection.t -> StateFormula.t option -> string -> Result.wrapped_result ok

module PropRes : Result.CustomResult with type value = propres

type t =
    {
        name : string;
        desc : string;
        check : StateFormula.Projections.Projection.t -> StateFormula.t -> PropRes.result;
        stats : StateFormula.SolverStats.t option
    }

val register : 
    string -> 
    string -> 
    (StateFormula.Projections.Projection.t -> StateFormula.t -> Result.wrapped_result ok) -> 
    StateFormula.SolverStats.t option ->
    unit
    (**[register name desc check stats] registers a new property [name] with the description [desc], the check function [check] and optional specific stats [stats]*)

val get : string -> t
(**get the property corresponding to the provided name*)

val get_list : unit -> (string * string) list

val incr : StateFormula.SolverStats.t -> StateFormula.Status.result -> unit
(**increments solver stats based on a result*)
