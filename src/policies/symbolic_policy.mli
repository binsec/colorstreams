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

(**symbolic execution*)

module S :
    sig
        type t

        val check_force : Sink.t -> bool -> t -> bool
    end

module SB : Symbolic.SymbolicBank.Sig
module SA : Analysis.Sig with type bank = SB.t and type state = S.t

module LibFunStubsBase :
    sig
        type state = SA.t
        type action = Update of state | OnRet of string * (Function.func list -> state -> state) | Both of state * string * (Function.func list -> state -> state) | Nothing
        type kind = Any
    end

(**better handling of library functions*)
module LibFunStubs :
    sig
        include Stubs.Sig with type state = LibFunStubsBase.state and type action = LibFunStubsBase.action and type kind = LibFunStubsBase.kind
    end

val init_analysis : 
    ?register_stats:bool ->
    ?concrete_w:string ->
    ?properties:(Symbolic.Properties.Property.t list) ->
    ?src_bytes:(SB.src_bytes_action * SB.byte list Utils.StringMap.t) ->
    ?wc_native:bool ->
    ?force:bool ->
    ?crwa:bool ->
    ?cull_after:int ->
    ?saww:int ->
    ?stubs:LibFunStubs.t ->
    ?dry:bool ->
    ?tag:(string option) ->
        string -> SA.t
(**[init_analysis ~register_stats ~concrete_w ~properties ~src_bytes ~wc_native ~force ~crwa ~cull_after ~saww ~stubs ~dry ~tag name] initializes an analysis with the provided [name]

- [register_stats] controls whether stats are globally displayed (on by default)
- [concrete_w] sets functions to be concretely executed (concrete writes) (format compatible with [Utils.pattern_list] with ';' separators)
- [properties] sets properties to be checked on positive sink
- [src_bytes] selects bytes from sources
- [wc_native] controls whether weak control is reduced to checking for symbolic bytes
- [force] forces properties to be checked even when sinks are not symbolic
- [crwa] controls whether all symbolic read and write addresses should be concretized (off by default)
- [cull_after] sets the number of seconds after which slow functions are concretized (only works when under monitoring with the "performance" monitor)
- [saww] sets the width of the symbolic memory address restriction window (in both directions)
- [stubs] sets the set of library function stubs to be used
- [dry] sets whether all sources should be ignore (fully concrete execution, off by default)
- [tag] sets the policy's tag
*)

module P : Policy.Sig with type p = SA.t
