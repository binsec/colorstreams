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

(**analysis framework*)

(**{1 Messaging}*)

(**analysis-related message wraps*)
module Wrap :
    sig
        type Result.custom_result += SinkRes of Sink.t * Result.wrapped_result
        type Message.ext_value += 
            | Snk of Sink.t
            | Src of Source.t
            | Ins of Instruction.t
            | Func of Function.t
            | WLoc of Trace.Gdb.LocRes.result * Message.ext_value
        
        type Message.Wrap.wrap += 
            | SinkResult of Sink.t * Result.wrapped_result (**result associated with a sink*)
            | Source of Source.t
            | Sink of Sink.t
            | Instruction of Instruction.t
            | Function of Function.t
            | WithLoc of Message.Wrap.wrap (**message with a stack trace*)
    end

(**messages for the monitoring of analyses*)
module Monitoring :
    sig
        type kind = ..

        type kind +=
            | Trc of Trace.trace
            | Snk of Sink.t
            | Src of Source.t
            | Ins of Instruction.t
            | Func of Function.t
            | Fini

        type status = ..

        type status += Start | Done

        type t = kind * status

        type Message.ext_value += Monit of t

        type Message.Wrap.wrap += Monitoring of t
    end

(**analysis-specific message handler*)
class mh : ?min_verb:int -> Identifier.t ->
    object
        inherit Message.handler

        method handle : Message.t -> Message.t option
        method set_min_verb : int -> unit
        (**sets a minimum verbosity for messages coming from child analysis policies*)
    end

(**{1 Analysis state}*)

(**template for generic analysis states (ex: {!Taint.MakeBank})*)
module type Bank =
    sig
        type t

        module Res : Result.CustomResult

        val create : unit -> t

        val add_stats_to_bundle : t -> Stats.Bundle.t -> Stats.Bundle.t

        val source : Source.t -> t -> t
        val sink : Sink.t -> t -> Res.result * bool

        val pp : t -> string
    end

(**dummy generic states*)
module NoneBank : Bank

(**template for specific analysis states (ex: parameters, sub-analyses...)*)
module type State =
    sig
        type t

        val create : ?tag:(string option) -> unit -> t
        val add_stats_to_bundle : Stats.Bundle.t -> t -> Stats.Bundle.t
    end

(**dummy specific states*)
module NoneState : State

(**{1 Generic analysis framework}*)

(**{2 Callbacks}*)

(**Analyses include five different types of analysis callbacks:
    - trace callbacks: called on any trace
    - source callbacks: called on sources
    - sink callbacks: called on sinks
    - instruction callbacks: called on instructions
    - function callbacks: called on function calls / returns and entry / exit points*)

(**In addition, end callbacks are called when analysis ends, either normally or due to an error.*)

(**{2 Guidelines}*)

(**Analysis policies should use {{!Colorstreams.Base.Analysis.Make} [Make]} to build a base with a relevant bank and a custom state if necessary.*)

(**Analyses should start with a {{!Colorstreams.Base.Analysis.Sig.FunctionCallbacks.start_callback}[start_callback]}, with all initialization done in its [todo].*)

module type Sig =
    sig
        type t
        type bank
        type state
        
        (**{1 Creation}*)

        val create : 
            ?filter_sources:(Source.t -> state -> bool) ->
            ?filter_sinks:(Sink.t -> state -> bool) ->
            ?filter_analyzed_sinks:(Sink.t -> bool -> state -> bool) -> 
            ?ignore_sources:bool ->
            ?ignore_sinks:bool ->
            ?monitoring:bool ->
            ?tag:(string option) ->
            string -> t

        (**{1 Modification}*)

        val get_name : t -> Identifier.t
        val get_bank : t -> bank
        val set_bank : t -> bank -> t
        val get_state : t -> state
        val set_state : t -> state -> t

        val with_message_handler : (unit -> 'a) -> t -> 'a

        val add_stats_to_bundle : Stats.Bundle.t -> t -> Stats.Bundle.t

        val set_filter_sources : (Source.t -> state -> bool) -> t -> t
        val set_filter_sinks : (Sink.t -> state -> bool) -> t -> t
        val set_filter_analyzed_sinks : (Sink.t -> bool -> state -> bool) -> t -> t
        val default_filter_sources : Source.t -> state -> bool
        val default_filter_sinks : Sink.t -> state -> bool
        val default_filter_analyzed_sinks : Sink.t -> bool -> state -> bool
        val set_ignore_sources : bool -> t -> t
        val set_ignore_sinks : bool -> t -> t
        val set_monitoring : bool -> t -> t

        val set_min_verbosity : int -> t -> t

        (**{1 Callbacks}*)

        module type CallbackQueue =
            sig
                type arg
                (**the argument type for callbacks*)

                val find : string -> t -> (arg -> t -> t)
                (**[find name a] returns the callback named [name] from [a] (if it exists, otherwise raises {!Not_found})*)

                val add : ?priority:int -> string -> (arg -> t -> t) -> t -> t
                (**[add ~priority name cb a] adds the callback [cb] named [name] with priority level [priority] to the analysis [a] 

                   [cb] may modify [a]*)

                val add_const : ?priority:int -> string -> (arg -> t -> unit) -> t -> t
                (**[add_const ~priority name cb a] adds the callback [cb] named [name] with priority level [priority] to the analysis [a]

                   [cb] may not modify [a]*)

                val remove : string -> t -> t
                (**[remove name a] removes the callback named [name] from [a]*)

                val fold : (string -> int -> (arg -> t -> t) -> 'a -> 'a) -> t -> 'a -> 'a

                val run : arg -> t -> t
                (**[run arg a] runs the callbacks from [a] with argument [arg] in decreasing order of priority*)
            end

        module TraceCallbacks :
            sig
                include CallbackQueue with type arg = Trace.trace
            end

        module SourceCallbacks :
            sig
                include CallbackQueue with type arg = Source.t
            end

        module SinkCallbacks :
            sig
                include CallbackQueue with type arg = Sink.t
            end

        module InstructionCallbacks : 
            sig
                include CallbackQueue with type arg = Instruction.t

                val bank_pp_callback : arg -> t -> unit
                (**logs the content of the analysis bank*)
            end

        (**also allows to set callbacks to be executed on return specifically*)
        module FunctionCallbacks :
            sig
                include CallbackQueue with type arg = Function.t

                val on_callret : ?priority:int -> Function.CallId.t -> string -> (Function.func list -> t -> t) -> t -> t
                (**[on_callret ~priority callid name cb a] adds the callback [cb] named [name] to [a] to be executed when returning from call [callid]

                   [cb] may modify [a]*)

                val on_callret_const : ?priority:int -> Function.CallId.t -> string -> (Function.func list -> t -> unit) -> t -> t
                (**[on_callret_const ~priority callid name cb a] adds the callback [cb] named [name] to [a] to be executed when returning from call [callid]

                   [cb] may not modify [a]*)

                val start_callback :
                    ?condition:(Function.t -> bool) ->
                    ?todo:(t -> t) ->
                    ?name:string ->
                    ?add_sink_checks:bool ->
                    ?add_source_checks:bool ->
                    ?fini_stats:bool ->
                    arg ->
                    t -> t
                (**[start_callback ~condition ~todo ~name ~fini_stats f a] executes [todo] if [condition f] is true, then removes itself

                 [name] should be the callback's name in [a] and [fini_stats] indicates whether a callback should be added to [a] to log its stats when analysis ends

                 [add_sink_checks] and [add_source_checks] are deprecated*)
            end

        module EndCallbacks : 
            sig
                include CallbackQueue with type arg = int
                (**the callback argument is either zero, a signal code or 100 in case of an uncaught exception*)

                val pp_stats_callback : arg -> t -> unit
                (**logs analysis stats*)
            end

        module SignalCallbacks :
            sig
                include CallbackQueue with type arg = Message.signal list
            end

        (**{1 Drivers}*)

        val analyze : Trace.trace -> t -> t
        (**runs all analysis callbacks on a trace*)

        val fini : int -> t -> t
        (**runs end callbacks

         the first argument sould be either zero, a signal code or 100 in case of an uncaught exception*)

        val options : string -> unit
        (**for internal use only*)
    end

module Make (B : Bank) (S : State) : Sig with type bank = B.t and type state = S.t

(**{1 Miscellaneous utilities}*)

module Utils :
    sig
        val strlen : Address.t -> Z.t
        (**equivalent to [strlen] from stdlib*)

        val get_ret_addr : unit -> Address.t
        (**get the return address in the current stack frame*)
    end
