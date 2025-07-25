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

(**local file IO*)

type t

val create : ?temporary:bool -> ?keep:bool -> string -> string -> t
(**[create ~temporary ~keep name ext] creates the representation of a file with named [name.ext]

[temporary] sets the file's directory to one in /tmp instead of the working directory (on by default)

[keep] allows the file to be saved to a non-temporary location if the -keep-tmp option is set (off by default)
*)

val from_file : ?create_dir:bool -> string -> t
(**[from_file ~create_dir path] creates the representation of a file based on [path]

[create_dir] enables the creation of the file's directory if it does not exist (off by default)
*)

val delete : t -> unit
(**[delete f] deletes f from the file system*)

val copy : t -> t -> unit
(**[copy f1 f2] copies f1 to f2*)

val get_dir : t -> string
val get_name : t -> string

val get_ext : t -> string
(**includes the dot*)

val get_path : t -> string
(**returns the full file path*)

val read : t -> string list
(**reads all lines in a file*)

val write : string -> t -> unit
(**writes a string to a file*)

val write_lines : string list -> t -> unit
(**writes a list of lines to a file*)

val open_out_chan : t -> out_channel
(**returns an output channel to the file*)

val open_in_chan : t -> in_channel
(**returns an input channel to the file*)
