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

(**natural number intervals*)

type t = 
    {
        lo : Z.t;
        hi : Z.t
    }
(**[lo <= n < hi]*)

val create : lo:Z.t -> hi:Z.t -> t

val compare : t -> t -> int
(**0 if intervals overlap*)

val equals : t -> t -> bool

val dist : t -> t -> Z.t
(**0 if intervals overlap*)

val pp : t -> string

val halve : t -> t list

val split : Z.t -> t -> t list
(**splits an interval arount a pivot value (not included in resulting intervals)

raises {!Failure} if the pivot is not in the interval
*)

val card : t -> Z.t
val contains : t -> Z.t -> bool

val pick_random : t -> Z.t
(**picks a random value in an interval*)

val overlap : t -> t -> bool
val subset : t -> t -> bool

val union : t -> t -> t
val inter : t -> t -> t
val sub : t -> t -> t option * t option
