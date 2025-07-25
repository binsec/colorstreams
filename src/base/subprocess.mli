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

(**subprocess handling*)

type session

val create_session : string -> string -> session

val get_name : session -> string
val get_pid : session -> int

val send : session -> string -> unit
val receive : session -> string
val receive_err : session -> string
val receive_until : session -> (string -> bool) -> string list
val receive_errs_until : session -> (string -> bool) -> string list
val receive_all : session -> string list
val receive_all_errs : session -> string list
val kill : session -> session
val close_session : session -> session

val log_errors : ?big:bool -> string -> session -> string list
