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

(**message passing and logging utilities*)

(**{1 Generic messages}*) 

val verbosity : int ref

type ext_value = ..
(**extend this to support custom message types*)

type ext_value += Value of string Lazy.t
type ext_value += Uncaught of string * string 

type t = 
    {
        verbosity : int;
        error : int option;
        ext_value : ext_value;
        value : string Lazy.t; (**pretty-print for logging*)
        prefix : string; (**message header to display*)
        handled_by : Identifier.t list (**list of encountered named handlers*)
    }

type signal = ..
(**extend this to add new signals*)

val is_error : t -> bool
val get_sender : t -> Identifier.t

(**{1 Message handling}*)

(**Base class for message handling*)
class handler : ?name:(Identifier.t option) -> unit ->
    object
        method handle : t -> t option
        method receive_signal : signal -> unit
        method get_signals : signal list
    end

(**logs last n messages on error if -bl is set*)
class backlogged_handler : ?name:(Identifier.t option) -> unit ->
    object
        inherit handler
    end

(**main logger*)
class logger :
    object
        inherit backlogged_handler
    end

(**for internal use only*)
class cleaner : 
    object
        inherit handler
    end

val accept : t -> unit
(**send a message through the message handling stack (almost never used directly)*)

val send_signal : signal -> unit
(**send a signal to the handler on top of the stack*)

val to_main : t -> unit
(**for internal use only*)

val with_handler : (unit -> 'a) -> handler -> 'a
(**[with_handler f h] runs [f] with the message handler [h]*)

val with_handlers : (unit -> 'a) -> handler list -> 'a
(**[with_handlers f l] runs [f] with the message handlers in [l]*)

val clean : unit -> unit
(**for internal use only*)

(**{1 Message wrapping}*)

(**For most purposes, messages sould be send wrapped.*)

module Wrap :
    sig
        type wrap = ..
        (**extend this type with custom wraps*)

        type wrap +=
            | BigInfo of string
            | Info of string Lazy.t
            | SmallInfo of string Lazy.t
            | Debug of string * string Lazy.t
            | SmallDebug of string * string Lazy.t
            | BigWarning of string Lazy.t
            | Warning of string Lazy.t

        type unwrapper = (wrap -> int * bool * string * ext_value * string Lazy.t) -> wrap -> int * bool * string * ext_value * string Lazy.t
        (**unwrappers take a default unwrapper and a wrap as argument and unwraps the wrap if it is known, otherwise it forwards it to the default unwrapper*)

        val register_unwrapper : unwrapper -> unit
        val unwrap : wrap -> int * bool * string * ext_value * string Lazy.t
        val mk_message : wrap -> t

        val send : wrap -> unit
        (**sends a wrapped message

        use this over {!accept}
         *)
    end
