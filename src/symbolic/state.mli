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

(**symbolic states*)

module type Sig =
    sig
        type t

        exception Desync of Storage.t list
        (**raised when constraints do not match the concrete values of the associated storages*)

        (**intra-instruction concrete control-flow guide*)
        module Guide :
            sig
                type guide

                val create : Instruction.t -> Dba.t -> guide
                (**creates a guide for an instruction*)

                val check_assert : Binsec.Dba.Expr.t -> guide -> bool
                (**checks a constraint on a guide*)

                val get_value : Binsec.Dba.Expr.t -> guide -> Z.t
                (**get a concrete value from a guide*)

                val step : (Binsec.Dba.Instr.t -> int -> 'a) -> guide -> guide * 'a
                (**[step f guide] updates the guide by one step and passes the next dba instruction's index to [f]*)

                val is_done : guide -> bool
                (**checks whether the guide reached the end of the instruction*)
            end

        val create : unit -> t

        val symbolize : ?tag:string -> Storage.t -> t -> t
        (**makes a storage unit's value symbolic (name controlled by [tag])*)

        val concretize : Storage.t -> t -> t
        (**concretizes a storage unit*)

        val assume : Storage.t -> t -> t
        (**assume a storage unit contains {e true}*)

        val is_symbolic : Storage.t -> t -> bool list
        (**checks whether a storage unit contains a symbolic value*)

        val assign_expr : Storage.t -> Expr.t -> t -> t
        (**assigns an expression to a storage unit*)

        val is_expr_symbolic : Expr.t -> t -> bool list
        (**checks whether an expression's value is symbolic*)

        val get_symbolic_inputs : Instruction.t -> t -> Storage.t list * Storage.t list * Storage.t list
        (**get all symbolic inputs of an instruction*)

        val next_instruction : ?crwa:bool -> ?saww:int -> Instruction.t -> Dba.t -> t -> t
        (**[next_instruction ~crwa ~saww instr dba state] updates [state] based on the given [dba]

        [crwa] controls whether read and write addresses are concretized
        [saww] sets the width of the restriction window for memory accesses at symbolic addresses (16 default, 0 is equivalent to crwa, use a negative value to disable)
        *)

        val make_input : string -> string list -> Storage.t -> t -> StateFormula.Inputs.Input.t * t
        (**creates a new input variable*)

        val get_inputs : t -> bool StateFormula.Inputs.t

        val make_projection : ?name:string -> ?suffix:string -> Expr.t -> t -> StateFormula.Projections.Projection.t * t
        (**creates a new projection of a variable*)

        val get_projections : t -> bool StateFormula.Projections.t
        val byte_restrict : ?lo:(int option) -> ?hi:(int option) -> Storage.t -> t -> t
        val pp_formula : t -> string
        val get_formula : ?proj_filter:((StateFormula.Projections.Projection.t -> bool) option) -> t -> StateFormula.t

        val add_stats_to_bundle : Stats.Bundle.t -> Stats.Bundle.t
    end

module Make () : Sig
