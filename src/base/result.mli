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

(**generic result framework*)

(**{1 Generic results}*)

type 'a bundle =
    {
        label : string;
        stats : Stats.Bundle.t option;
        result : 'a;
        custom_printer : string list Lazy.t option
    }

type generic = Bool of bool | Int of Z.t | Flt of float | Itv of Interval.t * Z.t * Z.t | Str of string | Sto of Storage.t | StoExpr of Expr.t | Lst of generic list | Res of generic bundle list

type t = generic bundle

val create : ?label:string -> ?stats:(Stats.Bundle.t option) -> generic -> t

val mk_bool : ?label:string -> ?stats:(Stats.Bundle.t option) -> bool -> t
val mk_int : ?label:string -> ?stats:(Stats.Bundle.t option) -> Z.t -> t
val mk_float : ?label:string -> ?stats:(Stats.Bundle.t option) -> float -> t
val mk_interval : ?label:string -> ?stats:(Stats.Bundle.t option) -> ?card:((Z.t * Z.t) option) -> Interval.t -> t
val mk_string : ?label:string -> ?stats:(Stats.Bundle.t option) -> string -> t
val mk_storage : ?label:string -> ?stats:(Stats.Bundle.t option) -> Storage.t -> t
val mk_sto_expr : ?label:string -> ?stats:(Stats.Bundle.t option) -> Expr.t -> t
val mk_list : ?label:string -> ?stats:(Stats.Bundle.t option) -> generic list -> t
val mk_res_list : ?label:string -> ?stats:(Stats.Bundle.t option) -> t list -> t

val set_custom_printer : string list Lazy.t option -> t -> t

val reslist_merge : t -> t

val add_stat : Stats.stat -> t -> t

val pp : ?pstats:bool -> t -> string

(**{1 Wrapped custom results}*)

type custom_result = ..
type wrapped_result = Generic of t | Custom of custom_result * t Lazy.t | Mixed of string * wrapped_result list
type Message.ext_value += Result of wrapped_result

val to_generic : wrapped_result -> t
val send : ?verbosity:int -> prefix:string -> wrapped_result -> unit

(**To create custom results, create a module of type {!CustomVal}, i.e., write a function converting your results to the generic result type, then use the {!Make} functor.*)

module type CustomVal =
    sig
        type t

        val to_generic : t -> generic
        (**converts a custom value to generic

        the resulting structure is used to pretty-print results by default
         *)
    end

module type CustomResult =
    sig
        type value
        type result = value bundle
        type custom_result += CustomResult of result

        val create : ?label:string -> ?stats:(Stats.Bundle.t option) -> value -> result
        val set_custom_printer : string list Lazy.t option -> result -> result

        val export : result -> t
        (**exports custom results to generic results*)

        val add_stat : Stats.stat -> result -> result

        val send : ?verbosity:int -> prefix:string -> result -> unit
        (**use this to log / report results through the {{!Colorstreams.Base.Message} [Message]} handling stack*)

        val wrap : result -> wrapped_result
        (**wraps a custom result*)
    end


module Make (V : CustomVal) : CustomResult with type value = V.t

(**for internal use only*)
class mh : unit ->
    object
        inherit Message.handler

        method handle : Message.t -> Message.t option
        method to_json : Yojson.Safe.t
    end

val load_from_file : File.t -> t
