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

(**trace handling*)

module Wrap :
    sig
        type Message.ext_value += private TargetCrashed | GeneralTimeout
    end

(**{1 Communication with GDB}*)

module Gdb :
    sig
        type session

        module ResVal :
            sig
                type resclass = Done | Running | Connected | Error | Exit
                type value = Const of string | Tuple of (string * value) list | List of value list | ResList of (string * value) list
                type t = 
                    {
                        token : int option;
                        cmd : string;
                        resclass : resclass;
                        res : (string * value) list
                    }
            end

        module Res : Result.CustomResult with type value = ResVal.t

        val get_locals : session -> string list
        (**get a list of local variables*)

        val get_globals : session -> (File.t * string) list
        (**get a list of defined global variables*)

        val eval_expr : ?silent:bool -> session -> string -> Z.t option
        (**evaluates an expression*)

        module Location :
            sig
                type t =
                    {
                        addr : Address.t option;
                        func : string option;
                        file : string option;
                        line : string option;
                        arch : string option;
                        caller : t option
                    }
            end

        module LocRes : Result.CustomResult with type value = Location.t

        val get_location_from_generic_result : Result.t -> LocRes.result

        val get_location : session -> LocRes.result option
        (**returns the current program location*)
    end

(**{1 Tracer interaction}*)

type trace = Ins of Instruction.t | Fun of Function.t | Src of Source.t | Snk of Sink.t | Ans of string | Tpl of string | Bbl of (Address.t * Z.t) list

val pp : trace -> string

val start : unit -> unit
val next : unit -> trace

val stoval : Storage.t -> Z.t
(**get the current value of a storage unit*)

val exprval : Expr.t -> Z.t
(**get the current value of an expression*)

val get_gdb : unit -> Gdb.session
(**CAUTION!!! any calls to the tracer will close the gdb session*)

val query_gdb : string -> Gdb.Res.result
val check_errors : unit -> unit

val terminate : unit -> unit
val terminated : unit -> bool

val terminate_next : unit -> unit
(**terminate the tracer after the current round of analysis*)

val kill : unit -> unit
val get_runtime : unit -> Stats.timer
val get_pid : unit -> int
val get_main_function : unit -> string
