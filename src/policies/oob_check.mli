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

(**automatic OOB detection*)

(**object pointer taint tags*)
module TT :
    sig
        type kind = Stack of string | Heap | Global of File.t * string | Unknown
        type t =
            {
                idx : int;
                kind : kind;
                base : Address.t;
                size : int
            }
    end

(**structure of results*)
module ResVal :
    sig
        (**type of written data*)
        type wdata = AfterUpdate (**to be checked after write*)
            | Expr of Expr.t (**equal to expression*)
            | Pattern of Expr.t (**repeated pattern*)
            | Input (**user input*)

        val pp_wdata : wdata -> string

        type rw = Read | Write of wdata

        (**type of detection*)
        type reason = TagOOB of TT.t (**OOB inferred from tag*)
            | TagUAF of TT.t (**UAF inferred from tag*)
            | Mapping of string (**memory mapping violation*)
            | Heap of string (**misc heap violation*)
            | Global of string (**misc global variable violation*)
            | Stack of string (**misc stack violation*)
            | Other

        type t =
            {
                id : int; (**detection number*)
                rw : rw;
                valid : bool; (**true if OOB does not violate permissions*)
                loc : Trace.Gdb.LocRes.result option Lazy.t; (**stack trace + location, lazy for internal optimization*)
                simple_loc : (string * Address.t option) option; (**for internal optimization*)
                target : Storage.t; (**actual read or write*)
                base : Expr.t;
                size : Expr.t;
                reason : reason
            }

        val pp_rw : rw -> string

        val similar : ?strict:bool -> t -> t -> bool
    end

module Res : 
    sig
        include Result.CustomResult with type value = ResVal.t

        val from_generic_result : Result.t -> result
    end

module A : Analysis.Sig

type t = A.t

module LibFunStubsBase :
    sig
        type state = t
        type action = Update of t | OnRet of string * (Function.func list -> t -> t)
        type kind = Check | Malloc | Both | Other
    end

(**improved handling of library functions*)
module LibFunStubs :
    sig
        include Stubs.Sig with type state = LibFunStubsBase.state and type action = LibFunStubsBase.action and type kind = LibFunStubsBase.kind
        
        module DefaultStubs :
            sig
                (**utilities usedful for implementing stubs*)
                module Utils :
                    sig
                        val do_alloc : string -> int -> Storage.t -> Address.t -> state -> state
                        (**[do_alloc name size sto ptr a] updates [a]'s shadow heap for the allocation a new object of size [size] and with a base address [ptr] stored in [sto]

                        [name] is used for debug messages
                        *)

                        val do_dealloc : string -> Storage.t -> Address.t -> state -> state
                        (**[do_dealloc name sto ptr a] updates [a]'s shadow heap for the deallocation of an object with base address [ptr] stored in [sto]
                        
                        [name] is used for debug messages
                         *)

                        val check_mem_one_way : Storage.t -> Expr.t -> Expr.t -> ResVal.rw -> Function.func -> Function.CallId.t -> state -> state
                        (**[check_mem_one_way destbasesto destexpr sizeexpr rw func callid a] checks a one-way read or write (e.g., memset) for OOBs, with [destbasesto] the location where the base address is stored, [destexpr] the base address and [sizeexpr] the size*)

                        val check_mem_two_way : Storage.t -> Expr.t -> Storage.t -> Expr.t -> Expr.t -> ResVal.wdata -> Function.func -> Function.CallId.t -> state -> state
                        (**[check_mem_two_way srcbasesto srcexpr destbasesto destexpr sizeexpr wdata func callid a] checks a two-way memory operation (e.g., memcpy) for OOBs, with [srcbasesto] the location where the source's base address is stored, [srcexpr] the source's base address, [destbasesto] the location where the destination's base address is stored, [destexpr] the destination's base address, [sizeexpr] the size and [wdata] the kind of written data*)
                    end
            end
    end

val default_filter : min_read_size:int -> reads_only_in:Str.regexp -> suppress_similar:bool -> Res.result -> bool

val init_analysis : 
    ?register_stats:bool -> 
    ?autokill:bool -> 
    ?dontcheck:(string list) -> 
    ?global_fallback:bool ->
    ?local_fallback:bool ->
    ?check_locals:bool ->
    ?ignore_small_reads:int ->
    ?cloif:string ->
    ?croif:string ->
    ?deteclim:int ->
    ?suppress_similar:bool ->
    ?filter:(Res.result -> bool) -> 
    ?stubs:LibFunStubs.t ->
    ?tag:(string option) ->
        string -> A.t
(**[init_analysis ~register_stats ~autokill ~dontcheck ~global_fallback ~local_fallback ~check_locals ~ignore_small_reads ~cloif ~croif ~deteclim ~suppress_similar ~filter ~stubs ~tag name] initalizes an analysis with the provided [name]

- [register_stats] controls whether stats are globally displayed (on by default)
- [autokill] controls whether analysis should be stopped when a memory mapping violation occurs (off by default)
- [dontcheck] sets a list of functions to ignore when checking OOBs
- [global_fallback] enables OOB check fallbacks for global variables (off by default)
- [local_fallback] enables OOB check fallbacls for local variables (off by default)
- [check_locals] controls whether stack objects are checked (on by default)
- [ignore_small_reads] ignore reads smaller than specified (-1 by default, disabled)
- [cloif] selects functions whose locals should be checked (string of ';' separated tokens compatible with {!Utils.pattern_list})
- [croif] selects functions where reads should be checked (idem)
- [deteclim] stop analysis after the specified number of detections (-1 by default, disabled)
- [suppress_similar] only report a single out of similar detections (larger sizes are still reported)
- [filter] sets a detection filter ({!default_filter} by default), overrides [ignore_small_reads], [croif] and [suppress_similar]
- [stubs] sets the stub set to be used
- [tag] sets the policy's tag
*)

module P : Policy.Sig with type p = A.t
