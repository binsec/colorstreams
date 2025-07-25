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

(**compute domains of control for a projection with Newsome et al.'s algorithm from the article "Measuring channel capacity to distinguish undue influence".*)

val check_range : StateFormula.Projections.Projection.t -> Interval.t -> StateFormula.t -> StateFormula.t * StateFormula.Status.result option
(**checks whether a projection can be within a given interval*)

val check_point : StateFormula.Projections.Projection.t -> Z.t -> StateFormula.t -> StateFormula.t * StateFormula.Status.result option
(**check whether a projection can take a given value*)

val bin_search_lower_bound : StateFormula.Projections.Projection.t -> Interval.t -> StateFormula.t -> Z.t
(**find the lower bound on a projection with binary search*)

val bin_search_upper_bound : StateFormula.Projections.Projection.t -> Interval.t -> StateFormula.t -> Z.t
(**find the upper bound on a projection with binary search*)

val density : int -> StateFormula.Projections.Projection.t -> Interval.t -> StateFormula.t -> float * float
(**compute a confidence interval on the density of values of a projection in a given interval (Wilson method)*)

type res = Point of Z.t | Density of Interval.t * float * float

module ResVal :
    sig
        type t =
            {
                feasible : res list;
                exact : Z.t;
                range : Z.t * Z.t
            }
    end

module Res : Result.CustomResult with type value = ResVal.t

val newsome : ?exhaust_lim:int -> ?density_lim:int -> ?timeout:int -> StateFormula.Projections.Projection.t -> StateFormula.t -> Result.wrapped_result Property.ok
(**[newsome ~exhaust_lim ~density_lim ~timeout proj sf] computes the domains of control for [proj] in [sf] with Newsome et al's algorithm

- [exhaust_lim] sets the number of feasible points to find (64 by default)
- [density_lim] sets the samle size for computing interval densities (64 by default)
- [timeout] sets a runtime limit (note: only stops analysis in-between density computations, hence overall runtime can be {e much} longer)
 *)
