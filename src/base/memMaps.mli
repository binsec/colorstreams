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

(**memory mappings*)

type desc = Spec of string (**named mapping (ex: stack)*)
    | File of File.t (**mapped file*)
    | Anon (**anonymous mapping*)

type entry =
    {
        range : Interval.t; (**address range*)
        permr : bool; (**read permission*)
        permw : bool; (**write permission*)
        permx : bool; (**execute permission*)
        foffset : int; (**file offset (if relevant)*)
        desc : desc
    }

type t = (desc * entry) list

val get : unit -> t
(**reads memory mappings from /proc/<pid>/maps*)

val where_sto : Storage.t -> t -> t
(**[where_sto sto maps] returns the mapping from [maps] containing [sto]*)

val where_file : string -> t -> t
(**[where_file name maps] returns the mapping from [maps] corresponding to the file named [name]*)

(*val first_file : t -> t*)

val check_perms : ?r:(bool option) -> ?w:(bool option) -> ?x:(bool option) -> entry -> bool
(**checks a mapping's permissions*)

val get_with_perms : ?r:(bool option) -> ?w:(bool option) -> ?x:(bool option) -> t -> t
(**get all mappings with the corresponding permissions*)

val entry_to_generic : entry -> Result.generic
val pp_entry_desc : entry -> string
val to_generic : t -> Result.generic

val is_kernel_space : entry -> bool
(**checks if a mapping lies beyond 2^48*)
