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

val register : string -> string -> (?tag:(string option) -> unit -> Message.handler) -> unit
(**[register name desc f] registers a monitor with name [name], description [desc] and a function [f] which creates a message handler for monitoring. The [tag] argument passes an option tag (the same as the overarching monitor policy).*)

module PerfMonitor :
    sig
        type Message.signal += Slow of string * Function.CallId.t
        (**slow function criteria: functions which have ran for at least 5% of the analysis runtime and which have a throughput 1000 times slower than the average*)
    end

module A : Analysis.Sig

val init_analysis :
    ?policies:((Policy.wrap * (module Policy.Sig)) list) ->
    ?monitors:(string list) ->
    ?tag:(string option) ->
        string -> A.t
(**[init_analysis ~policies ~monitors ~tag name] creates a monitoring analysis named [name]

- [policies] sets the underlying policies (only policies among those which have the [monitoring] flag set are monitored)
- [monitors] sets the monitors to be used
- [tag] sets the option tag of analysis*)

module P : Policy.Sig with type p = A.t
