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

(**expressions over storage units*)

type unop = Restrict of int * int | Extend of int | Strlen of Z.t (**length of string at given address*)
(**unary operators*)

type bnop = Add | Sub | Mul | Max | Min | Dist
(**binary operators*)

type t = 
    | Const of Z.t * int (**constant*)
    | Var of Storage.t
    | Unop of unop * t
    | Bnop of bnop * t * t

val pp_unop : unop -> string
val parse_unop : string -> unop
val pp_bnop : bnop -> string
val parse_bnop : string -> bnop
val pp : t -> string
val size : t -> int
val is_const : t -> bool

val get_stos : t -> Storage.t list
(**[get_stos e] returns the list of all storage units present in [e]*)

val simplify : t -> t
(**simplifies an expression by propagating constants etc...*)
