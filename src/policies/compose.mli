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

(**analysis policy composition for sources and sinks*)

module A : Analysis.Sig

val init_analysis :
    ?producers:((Policy.wrap * (module Policy.Sig)) list) ->
    ?consumers:((Policy.wrap * (module Policy.Sig)) list) ->
    ?allow_ext_src:bool ->
    ?allow_ext_snk:bool ->
    ?tag:(string option) ->
        string -> A.t
(**[init_analysis ~producers ~consumers ~tag name] initializes an analysis with the provided [name]

- [producers] sets the source and sink producing analyses
- [consumers] sets the source and sink consuming analyses
- [allow_ext_src] sets whether external sources are transmitted to consumers
- |allow_ext_snk] sets whether external sinks are transmitted to consumers
- [tag] sets the policy's tag
*)

module P : Policy.Sig with type p = A.t
