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

(**miscellaneous utilities*)

val pattern_list : string list -> Str.regexp

val pp_list : ?sep:string -> ('a -> string) -> 'a list -> string

val z_of_bytes : Z.t list -> Z.t
val bytes_of_z : Z.t -> int -> Z.t list

val z_to_int : Z.t -> int

module StringMap : Map.S with type key = string
module IntMap : Map.S with type key = int
module IdMap : Map.S with type key = Identifier.t
module ZMap : Map.S with type key = Z.t
