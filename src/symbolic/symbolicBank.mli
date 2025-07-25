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

(**analysis bank for symbolic execution*)

module type Sig =
    sig
        module S : State.Sig

        type mode_t = Symbolic | Concrete of int | Concrete_until of Function.CallId.t | Concrete_next | Wait_source

        type tag = Symb

        type byte = Single of int | Range of int * int
        type src_bytes_action = Restrict | Only

        include Analysis.Bank with type Res.value = tag option list

        val set_crwa : bool -> t -> t
        (**sets whether read and write addresses should be concretized*)

        val set_saww : int -> t -> t
        (**sets the width of the restriction window for symbolic addresses*)

        val set_src_bytes : ?action:src_bytes_action -> byte list Utils.StringMap.t -> t -> t
        (**sets which bytes of sources should be made symbolic*)

        val get_mode : t -> mode_t
        val get_runtime : t -> float
        val get_state : t -> S.t
        val get_sink_formula : Sink.t -> t -> (StateFormula.Projections.Projection.t * StateFormula.t) option

        val set_concrete : string list -> t -> t
        (**sets which functions should be concretely executed*)

        val add_concrete : string -> t -> t
        (**adds a concrete function*)

        val set_state : S.t -> t -> t
        val switch_mode : mode_t -> t -> t
        val next_instruction : Instruction.t -> t -> t
        val next_function : Function.t -> t -> t

        val assign_expr : Storage.t -> Expr.t -> t -> t
        (**assigns an expression to a storage unit*)
    end

module Make (State : State.Sig) : Sig

