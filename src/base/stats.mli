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

(**statistics*)

(**Stats are objects by design in order to allow for in-place modifications.
 This ensures that they remain globally consistent across analysis states.*)

class virtual stat : ?visible:bool -> string ->
    object
        method name : string
        method visible : bool
        method virtual pp : string
        method virtual to_json : Yojson.Safe.t
    end

class sstat : ?visible:bool -> string -> string ->
    object
        inherit stat
        method get : string
        method set : string -> unit
        method pp : string
        method to_json : Yojson.Safe.t
    end

class flag : ?visible:bool -> string -> string ->
    object
        inherit stat
        method get : bool
        method set : unit
        method unset : unit
        method pp : string
        method to_json : Yojson.Safe.t
    end

class counter : ?visible:bool -> ?callback:(int -> unit) -> string -> string -> int ->
    object
        inherit stat
        method get : int
        method set : int -> unit
        method incr : unit
        method decr : unit
        method pp : string
        method to_json : Yojson.Safe.t
    end

class timer : ?visible:bool -> string -> string ->
    object
        inherit stat
        method running : bool
        method start : unit
        method stop : unit
        method get : float
        method reset : unit
        method pp : string
        method to_json : Yojson.Safe.t
    end

(**named set of stats*)
module Bundle :
    sig
        type t

        val create : ?report:bool -> string -> t
        val get_name : t -> string
        val add_stat : stat -> t -> t
        val get_stat : string -> t -> stat
        val pp : t -> string
        val pp_stats : t -> string list
        val to_json : t -> Yojson.Safe.t
    end

(**message wrapping*)
module Wrap :
    sig
        type Message.ext_value += ExtStats of Bundle.t
        type Message.Wrap.wrap += 
            | Stats of Bundle.t
            | BigStats of Bundle.t
    end

module GeneralStats :
    sig
        val runtime : timer
        val warn_cnt : counter
        val cnt : counter
    end

val register : ?big:bool -> ?silent:bool -> Bundle.t -> unit
(**register stats to be displayed dynamically when -ls is enabled*)

val log : unit -> unit
val to_json : unit -> Yojson.Safe.t
