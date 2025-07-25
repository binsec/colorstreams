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

(**advanced taint analysis at DBA level*)

(**taint tags*)
module TT :
    sig
        type kind = Direct (**direct data dependencies*)
            | Indirect (**{e disabled} indirect data dependencies (e.g., from memory read address)*)
            | Control (**{e disabled} control dependencies*)

        type t = 
            {
                src : Source.t;
                kind : kind;
                byte_collection : Collection.t; (**from which bytes of a same source the tag originates*)
            }

        val pp : t option -> string
    end

module MTT : Taint.MultiTagSig with type tt = TT.t
module TB : Taint.TaintBank with type tt = MTT.t
module A : Analysis.Sig with type bank = TB.t

val init_analysis :
    ?register_stats:bool ->
    ?indirect:bool ->
    ?control:bool ->
    ?flags:bool ->
    ?drop:string ->
    ?tag:(string option) ->
        string -> A.t
(**[init_analysis ~register_stats ~indirect ~control ~flags ~drop ~tag name] initializes an analysis with the provided [name]

- [register_stats] controls whether stats are diplayed globally (on by default)
- [indirect] controls whether taint should be propagated along indirect dependencies
- [control] controls whether taint should be propagated from conditions of DBA if statements
- [flags] controls whether taint should be propagated to flags
- [drop] sets functions where taint should be dropped (format compatible with [Utils.pattern_list] with ';' separators)
- [tag] sets the policy's tag
 *)

module P : Policy.Sig with type p = A.t
