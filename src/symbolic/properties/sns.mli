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

(**compute domains of control with Shrink and Split*)

type itv = Strong of Interval.t (**strong control proven*)
    | MaybeStrong of Interval.t (**finding a counterexample failed*)
    | NotStrong of Interval.t (**weak*)
    | Unknown of Interval.t (**error*)

val get_itv : itv -> Interval.t

module ResVal :
    sig
        type t =
            {
                constrprop : string;
                constrres : Result.wrapped_result Property.ok option; (**additional constraints*)
                size : int;
                card : itv -> Interval.t Lazy.t; (**computes the cardinal of an interval (with additional constraints)*)
                itvs : (itv * Interval.t Lazy.t) list; (**intervals + cardinals*)
                min : Z.t Lazy.t; (**min value count*)
                maybe_min : Z.t Lazy.t; (**min value count if maybestrong intervals are strong*)
                max : Z.t Lazy.t; (**max value count*)
            }

        val cut : Interval.t -> t -> t * t
        (**cuts domains around an interval

        returns {e inside domain}, {e outside domain}
         *)

        val intersect : t -> t -> t option
        (**[intersect domain1 domain2] intersects two domains taking regularity constraints into account.

         Returns {!None} if the intersection is empty and throws {!Failure} if the operation cannot be done.*)

        val constr_builder : ?filter:(itv -> itv list) -> t -> Binsec.Formula.var -> Binsec.Formula.term list -> Binsec.Formula.bl_term
    end

module Res : Result.CustomResult with type value = ResVal.t

val domains_from_generic_results : Result.t -> Res.result
(**[domains_from_generic_results result]

Important note: PMC counts and projection constraints other than regularity constraints will not carry over!*)

(**additional regularity constraints*)
module Constraints :
    sig
        type t = StateFormula.Projections.Projection.t -> StateFormula.t -> StateFormula.t * Result.wrapped_result Property.ok option

        val register : string -> string -> string -> t -> (Result.wrapped_result Property.ok -> StateFormula.constr_builder option) -> (Result.t -> Result.wrapped_result Property.ok option) -> (Result.wrapped_result Property.ok -> Result.wrapped_result Property.ok -> Result.wrapped_result Property.ok option) -> unit
        (**[register name desc suff f g from_generic intersect] registers a new type of additional regularity constraints [name] for S&S with description [desc], S&S suffix [suff], building functions [f] and [g], a function [from_generic] parsing results from generic values and a function [intersect] which produces the intersection of two constraints.
           - [f] checks the property on the projection in the formula, attaches the constraint to the projection and returns the results
           - [g] creates a simple constraint builder from a result*)

        val get : string -> t

        val get_constr_builder : string -> (Result.wrapped_result Property.ok -> StateFormula.constr_builder option)

        val get_parser : string -> (Result.t -> Result.wrapped_result Property.ok option)
    end

val sns : ?split_max:int -> ?timeout:int -> ?constrprop:string -> ?force_binsearch_bounds:bool -> StateFormula.Projections.Projection.t -> StateFormula.t -> Res.result
(**[sns ~split_max ~timeout ~constr ~force_binsearch_bounds proj sf] computes the domains of control for projection [proj] in [sf] with Shrink and Split

- [split_max] sets a split limit (none (-1) by default)
- [timeout] sets a runtime limit
- [constrprop] selects additional regularity contraints
- [force_binsearch_bounds] forces the binary search fallback for finding feasible bounds in intervals, which can be useful if Z3 is performing especially poorly
*)
