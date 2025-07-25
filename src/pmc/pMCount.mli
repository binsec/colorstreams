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

(**PMC solver interaction*)

module CountVal :
    sig
        type t = Count of Z.t | Timeout | Error
    end

module Count : Result.CustomResult with type value = CountVal.t

module Solver :
    sig
        val timeout : int ref

        val register : string -> string -> (projections:(string list) -> File.t -> string) -> (string list * string list * Count.result  -> Count.result) -> unit
        (**[register name desc mk_cmd parse_out] registers a solver called [name] with description [desc], [mk_cmd] a function building its command lines and [parse_out] a function parsing its output and errors into a model count*)

        val query : ?solver:string -> projections:(string list) -> File.t -> Count.result list
        (**[query ~solver ~projections file] counts models in [file] for projected variables [projections] with [solver]*)
    end

module Popcon :
    sig
        val get_cnf : projections:(string list) -> File.t -> string list * string list
        (**converts an smt2 formula into a cnf formula using popcon*)
    end
