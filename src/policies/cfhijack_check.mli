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

(**automatic Control-Flow Hijack detection*)

module ResVal :
    sig
        type kind = Call | Jump | Ret

        type t =
            {
                id : int; (**detection number*)
                loc : Trace.Gdb.LocRes.result option; (**stack trace + location*)
                target : Storage.t; (**read storage*)
                kind : kind;
                taint : Bytedep_policy.MTT.t option list
            }

        val pp_kind : kind -> string
    end

module Res : Result.CustomResult with type value = ResVal.t

module A : Analysis.Sig

val init_analysis :
    ?register_stats:bool ->
    ?tag:(string option) ->
        string -> A.t
(**[init_analysis ~register_stats ~tag name] initializes an analysis with the provided [name]

- [register_stats] constrols whether stats are globally displayed (on by default)
- [tag] sets the policy's tag
*)

module P : Policy.Sig with type p = A.t
