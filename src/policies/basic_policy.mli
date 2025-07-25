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

(**basic taint analysis*)

(**taint tags*)
module TT :
    sig
        type t = Controlled | Uncontrolled

        val pp : t option -> string
    end

module TB : Taint.TaintBank with type tt = TT.t
module A : Analysis.Sig with type bank = TB.t

val init_analysis :
    ?register_stats:bool ->
    ?dump_taint:bool ->
    ?check_overtaint:bool ->
    ?tag:(string option) ->
        string -> A.t
(**[init_analysis ~register_stats ~dump_taint ~check_overtaint ~tag name] initalizes an analysis with the provided [name]

- [register_stats] controls whether stats are displayed globally (on by default)
- [dump_taint] controls whether the taint bank's content is logged (off by default, cli)
- [check_overtaint] controls whether taint is compared to symbolic bytes with symbolic execution (off by default, cli)
- [tag] sets the policy's tag
 *)

module P : Policy.Sig with type p = A.t
