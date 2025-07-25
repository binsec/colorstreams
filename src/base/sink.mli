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

(**data flow sinks*)

type ext_kind = ..
(**extend this type with new automatic sink types*)

type kind = Custom | Auto of ext_kind * string

type t =
    {
        name : Identifier.t;
        expr : Expr.t; (**variable expression*)
        fname : string; (**parent function name*)
        kind : kind;
        more : string; (**text description*)
        constr : Storage.t option (**validity constraint*)
    }

val create : ?dummy:bool -> ?fname:string -> ?kind:kind -> ?more:string -> ?constr:(Storage.t option) -> string -> Expr.t -> t
(**Use the [dummy] option to create sinks without incrementing the global counter.*)

val parse : string -> t
val has_constr : t -> bool
val pp : t -> string
val compare : t -> t -> int
val equals : t -> t -> bool
