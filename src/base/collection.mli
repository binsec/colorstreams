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

(**sets of intervals*)

type t

val empty : t

val single : Z.t -> t
(**creates a collection with a singleton*)

val interval : Interval.t -> t
(**creates a collection with a single interval*)

val equals : t -> t -> bool

val insert : Z.t -> t -> t
(**[insert n c] inserts the singleton [{n}] in [c]*)

val insert_interval : Interval.t -> t -> t
(**[insert_interval i c] inserts the interval [i] in [c]*)

val contains : Z.t -> t -> bool

val merge : t -> t -> t
(**merges two collections into one*)

val pp : t -> string
