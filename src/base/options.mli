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

(**command line options*)

(**policy options which can be tag-specific*)
module TaggedOptions :
    sig
        class ['a] opt : 'a ->
            object
                method get : ?tag:string -> 'a
                method maybe_get : ?tag:string -> 'a option -> 'a
                method default : 'a
                method set : ?tag:string -> 'a -> unit
            end

        type t = Untagged of (unit -> unit) (**for options that do not need to be tag-specific*)
            | Bool of bool opt (**flags (set to the opposite of the default value)*)
            | String of string opt (**string options*)
            | Int of int opt (**int options*)
    end

val get_target : unit -> string
(**returns the target program*)

(**for internal use only*)
val parse_args : unit -> bool

(**for internal use only*)
val parse_args_argv : string array -> unit

val pp_help : unit -> string

val register_policy_option : string -> string -> TaggedOptions.t -> string -> unit
(**[register_policy_option policy name opt desc] registers the option [-policy-name] for a policy, with tagging support*)

val register_option : (string * Arg.spec * string) -> unit

val register_policy_alias : string -> string -> string -> unit
val register_alias : string -> string -> unit

val register_todo : (unit -> unit) -> unit
(**for internal use only*)

val has_todos : unit -> bool
(**for internal use only*)

val do_todos : unit -> unit
(**for internal use only*)
