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

(**memory addresses*)

type t = Z.t

val compare : t -> t -> int
val equal : t -> t -> bool
val of_string : string -> t
val zero : t

val incr : t -> t
(**[incr a] increments [a] by one byte*)

val add_int : t -> int -> t
(**[add_int a n] increments [a] by [n] bytes*)

val to_string_hex : t -> string
(**converts an address to string in hexadecimal format*)

val to_bv : t -> Binsec.Bitvector.t
(**converts an address to a binsec bitvector*)
