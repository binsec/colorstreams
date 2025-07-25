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

(**analysis policies*)

(**generic policy base*)
module type BaseSig =
    sig
        type p

        module A : Analysis.Sig with type t = p

        val name : string 
        (**name of the policy*)

        val desc : string 
        (**description of the policy*)

        val init : ?tag:(string option) -> unit -> p
        (**creates an analysis with the specified tag*)
    end

type wrap = ..

(**generic policies*)
module type Sig =
    sig
        include BaseSig

        val wrap : p -> wrap
        (**used to despecialize analyses, for example to pass a manually created analysis to another which expects generic ones (such as {{!Colorstreams.Policies.Compose.init_analysis} [compose]})*)

        val update : (A.t -> A.t) -> wrap -> wrap
        val apply : (A.t -> 'a) -> wrap -> 'a
    end

module Make (Base : BaseSig) : Sig with type p = Base.p

val register_policy : (module Sig) -> unit
(**registers policies to be available from the command line*)

val parse : string -> (string * string option) list
(**parses policy specs*)

val get : string -> (module Sig)

val init : unit -> (wrap * (module Sig)) list
(**for internal use only*)
