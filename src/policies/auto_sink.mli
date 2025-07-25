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

(**sink generation with function stubs*)

module A : Analysis.Sig

module SinkStubsBase :
    sig
        type state = A.t
        type action = Sink of Sink.t | OnRet of string * Sink.t Lazy.t | NextInstr of string * Sink.t Lazy.t | Nothing
        type kind = SnkKind
    end

(**stubs generating sinks*)
module SinkStubs :
    sig
        include Stubs.Sig with type state = SinkStubsBase.state and type action = SinkStubsBase.action and type kind = SinkStubsBase.kind

        module DefaultStubs :
            sig
                val buf : string -> Stubs.GdbArgs.t -> Stubs.GdbArgs.t -> string -> Function.func -> Function.CallId.t -> state -> action
                (**[bytes_to_buf point buf size name func callid state]* generates a sink corresponding to a buffer with base address [buf] and [size], named [name], at [point = entry] or [point = ret]

                [callid] and [state] are unused
                *)

                val ret_reg : Function.func -> Function.CallId.t -> state -> action
                (**[ret_reg func callid state] generates a sink corresponding to the return value register

                [callid] and [state] are unused
                *)

                val arg_reg : int -> Function.func -> Function.CallId.t -> state -> action
                (**[arg_reg argnum func callid state] generates a sink corresponding to the function argument number [argnum]

                [callid] and [state] are unused
                 *)
            end
    end

val init_analysis : ?restrict:((int * int) Utils.StringMap.t) -> ?stubs:SinkStubs.t -> ?tag:(string option) -> string -> A.t
(**[init_analysis ~restrict ~stubs ~policies ~tag name] initializes an analysis with the provided [name]

- [restrict] sets sink restrictions to a smaller byte interval
- [stubs] allows to provide a custom set of stubs and stub attributions
- [tag] sets the policy's tag
 *)

module P : Policy.Sig with type p = A.t
