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

(**Function stubbing utilities*)

module type Base =
    sig
        val modulename : string

        type state
        type action
        type kind

        val pp_kind : kind -> string

        val check_kind : Function.func -> Function.CallId.t -> state -> kind -> state * bool
        (**checks that the stub should be run based on its kind and the analysis state*)

        val act : Function.func -> Function.CallId.t -> state -> action -> state
        (**perform stub actions*)
    end

module type Sig =
    sig
        include Base

        type stub = Function.func -> Function.CallId.t -> state -> action

        type parsr = string list -> stub
        (**stub parsers, for parsing command line stub specification

        takes a list of arguments as input
         *)

        type t

        val register : string -> string list -> string -> kind -> parsr -> unit
        (**[register name spec desc kind parsr] registers a new stub called [name] with the argument specification [spec] and the description [desc] (both for display). [kind] is its {!kind} and [parsr] is a function for parsing command-line specification.*)

        val register_default : string -> string -> kind -> stub -> unit
        (**registers a default stub for a function*)

        val get_parsr : string -> kind * parsr

        val pp : t -> string

        val create : string -> t
        (**creates an empty set of stubs*)

        val run : t -> Function.t -> state -> state
        (**run stubs for a function, if any are set*)

        val cli_specs : string Options.TaggedOptions.opt
        (**command line specs (from -<policy>-<name>-stubs*)

        val register_options : string -> unit
        (**generate default options for a policy*)

        val basic_parsr : stub -> parsr
        (**basic parser for stubs without parameters*)
    end

module Make (B : Base) : Sig with type state = B.state and type action = B.action and type kind = B.kind

(**stub arguments with gdb expression evaluation support*)
module GdbArgs :
    sig
        type t

        val spec : string
        (**for stub descriptions*)

        val parse : string -> t
        (**for stub parsers*)

        val get : Function.func -> t -> Z.t Lazy.t
        (**to be evaluated on function return*)
    end
