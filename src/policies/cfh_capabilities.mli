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

(**analysis of CFH capabilities*)

(**structure of results*)
module ResVal :
    sig
        type param = Oob_capabilities.ResVal.param

        module Triple : module type of Oob_capabilities.ResVal.Triple

        type score = Triple.t

        type report = ..
        (**extend with new report types*)

        type t =
            {
                detection : Cfhijack_check.Res.result;
                maps : MemMaps.t;
                ptr : param;
                report : (report * Result.generic Lazy.t) option
            }

        module Reporters :
            sig
                val register : string -> (t -> (report * Result.generic Lazy.t) option) -> unit
            end

        (**default report generator*)
        module DefaultReporter :
            sig
                type entry =
                    {
                        map : MemMaps.entry;
                        ptrdomain : Symbolic.Properties.Sns.ResVal.t;
                        score : score
                    }

                type report += Default of entry list * score
            end
    end

module Res : Result.CustomResult with type value = ResVal.t

module A : Analysis.Sig

type t = A.t

val init_analysis :
    ?tag:(string option) ->
    ?selected_detections:(int list) ->
    ?reporter:string ->
        string -> t
 (**[init_analysis ~tag ~selected_detections ~reporter name] initializez an analysis with the provided [name]

- [tag] sets the policy's tag
- [selected_detections] restricts analyzed detecions by their number (no restrictions by default)
- [reporter] sets the report generator to be used (default reported by default)
 *)

 module P : Policy.Sig with type p = A.t
