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

(**source generation with function stubs*)

module A : Analysis.Sig

module SourceStubsBase :
    sig
        type state = A.t
        type action = Source of Source.t | OnRet of string * Source.t Lazy.t | NextInstr of string * Source.t Lazy.t | Nothing
        type kind = SrcKind
    end

(**stubs generating sources*)
module SourceStubs :
    sig
        include Stubs.Sig with type state = SourceStubsBase.state and type action = SourceStubsBase.action and type kind = SourceStubsBase.kind

        module DefaultStubs :
            sig
                val bytes_to_buf : string -> Stubs.GdbArgs.t -> Stubs.GdbArgs.t -> string -> string list -> Function.func -> Function.CallId.t -> state -> action
                (**[bytes_to_buf point buf size name desc func callid state]* generates a source corresponding to a buffer with base address [buf] and [size], named [name] and with description [desc], at [point = entry] or [point = ret]

                [callid] and [state] are unused
                *)

                val cmd_arg : string -> string -> string list -> Function.func -> Function.CallId.t -> state -> action
                (**[cmd_arg argnum name desc func callid state] generates a source for command line argument number [argnum], named [name] and with description [desc]

                [callid] and [state] are unused
                *)

                val ret_reg : string list -> Function.func -> Function.CallId.t -> state -> action
                (**[ret_reg desc func callid state] generates a source corresponding to the return value register with description [desc]

                [callid] and [state] are unused
                *)

                val arg_reg : int -> string list -> Function.func -> Function.CallId.t -> state -> action
                (**[arg_reg argnum desc func callid state] generates a source corresponding to the function argument register number [argnum] with description [desc]

                [callid] and [state] are unused
                *)
            end
    end

val init_analysis : ?stubs:SourceStubs.t -> ?tag:(string option) -> string -> A.t
(**[init_analysis ~stubs ~policies ~tag name] initializes an analysis with the provided [name]

- [stubs] allows to provide a custom set of stubs and stub attributions
- [tag] sets the policy's tag
 *)

module P : Policy.Sig with type p = A.t
