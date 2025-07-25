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

(**function call / return site and entry / exit point representation and call stack state tracking*)

(**{1 Calling conventions}*)

module CallConv :
    sig
        type t
    end

(**{1 Call identifiers}*)

module CallId :
    sig
        type t =
            {
                site_id : int;
                stack_id : int
            }

        val create : int -> int -> t

        val compare : t -> t -> int
        val pp : t -> string
    end

(**{1 Functions}*)

(**function info*)
type func = 
    {
        fname : string;
        cconv : CallConv.t;
        call_id : CallId.t option
    }

(**call stack state*)
type state

val empty_state : state

(**call / return, entry / exit point*)
type t = Call of CallId.t | Entry of func | Exit of func | CallRet of CallId.t * func list

val get_fname : t -> string option

val parse : string -> state -> t * state
val pp : t -> string

val get_iarg : int -> func -> Storage.t
(**[get_iarg n f] returns a storage unit corresponding to the nth argument of [f]*)

val get_iret : int -> func -> Storage.t
(**[get_iret n f] returns a storage unit corresponding to the nth return register of [f]*)

val get_toplevel : state -> string
(**[get_toplevel state] returns the currently executing function*)
