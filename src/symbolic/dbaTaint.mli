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

(**taint analysis at DBA level*)

type action = Propagate (**use advanced DBA-level propagation*)
    | Fallback (**perform user-provided fallback propagation*)
    | Erase (**erase all taint in written storage units*)

module type TBSig =
    sig
        include Taint.TaintBank

        val direct : tt option -> tt option
        (**controls direct taint tags (e.g., can filter them out)*)

        val indirect : tt option -> tt option
        (**controls indirect taint tags, from read / write addresses (e.g., can filter them out)*)

        val control : tt option -> tt option
        (**controls control taint tags (e.g., can filter them out)*)

        val unary_taint : Binsec.Dba.Expr.t -> Binsec.Dba.Unary_op.t -> Binsec.Dba.Expr.t -> tt option list -> tt option list
        (**controls how taint is propagated through unary operators*)

        val binary_taint : Binsec.Dba.Expr.t -> Binsec.Dba.Binary_op.t -> Binsec.Dba.Expr.t -> tt option list -> Binsec.Dba.Expr.t -> tt option list -> tt option list
        (**controls how taint is propagated through binary operators*)

        val byte_bit_merge : tt option -> tt option -> tt option
        (**controls how bit-wise taint tags of a same byte are converted to a single byte-wise taint tag*)

        val bit_merge : tt option -> tt option -> tt option
        (**controls how bit-wise taint tags are merged*)

        val decide : Instruction.t -> Storage.t list -> t -> action
        (**controls how instructions are handled

        the second argument is a list of input operands to be considered
         *)

        val fallback : Instruction.t -> t -> t
        (**fallback propagation (used when disassembly fails or {!decide} calls for it*)
    end

module Make (TB : TBSig) :
    sig
        include TBSig with type t = TB.t and type tt = TB.tt

        (**DBA-level taint propagation

        flag, indirect and control taint propagation are disabled by default
         *)
        val propagation : ?propagate_flags:bool -> ?propagate_indirect:bool -> ?propagate_control:bool -> Instruction.t -> t -> t

        val erase : Instruction.t -> t -> t
        (**erase taint in written locations*)
    end
