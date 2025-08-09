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

(**analysis of OOB capabilities*)

(**structure of results*)
module ResVal :
    sig
        type param = 
            {
                snk : Sink.t;
                domain : Symbolic.Properties.Sns.Res.result
            }

        module Triple :
            sig
                type t =
                    {
                        min : float;
                        maybe_min : float;
                        max : float
                    }

                val zero : t
                val apply_float : (float -> float -> float) -> t -> float -> t
                val apply_triple : (float -> float -> float) -> t -> t -> t
                val of_domain : Symbolic.Properties.Sns.ResVal.t -> t
                val to_generic : t -> Result.generic
            end

        type report = ..
        (**extend with new report types*)

        type t =
            {
                detection : Oob_check.Res.result;
                maps : MemMaps.t;
                base : param option;
                offset : param option;
                size : param option;
                data : param list;
                report : (report * Result.generic Lazy.t) option
            }

        (**report generators*)
        module Reporters :
            sig
                val register : string -> string -> (?similar:((string * t) list) -> t -> (report * Result.generic Lazy.t) option) -> unit
            end

        (**default report generator*)
        module DefaultReporter :
            sig
                type score =
                    {
                        base : Triple.t;
                        size : Triple.t;
                        data : Triple.t option;
                        overall : Triple.t
                    }

                module MapKey :
                    sig
                        type t = MemMaps.entry

                        val compare : t -> t -> int
                    end

                module MapMap : Map.S with type key = MapKey.t

                type default_report = 
                    {
                        alone : score MapMap.t;
                        similar : (int * string) list;
                        full : score MapMap.t option
                    }
        
                type report += Default of default_report
            end
    end

module Res : Result.CustomResult with type value = ResVal.t    

module A : Analysis.Sig

type t = A.t

val init_analysis :
    ?tag:(string option) ->
    ?selected_detections:(int list) ->
    ?reporter:string ->
    ?load:string ->
        string -> t
(**[init_analysis ~tag ~selected_detections ~reporter ~load name] initializes an analysis with the provided [name]

- [tag] sets the policy's tag
- [selected_detections] restricts analyzed detections by their number (no restrictions by default)
- [reporter] sets the report generator to be used (default reporter by default)
- [load] is a file path to load old results from
*)

module P : Policy.Sig with type p = A.t
