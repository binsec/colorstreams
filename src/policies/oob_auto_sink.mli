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

(**generation of sinks based on OOB detections*)

type Sink.ext_kind +=
    | OOBBase of Oob_check.Res.result (**OOB base address*)
    | OOBSize of Oob_check.Res.result (**OOB size*)
    | OOBWData of Oob_check.Res.result (**written data (immediate)*)
    | OOBWDataNext of Oob_check.Res.result (**written data (checked after write)*)

module A : Analysis.Sig

val init_analysis : ?data_lim:int -> ?split_data:bool -> ?tag:(string option) -> string -> A.t
(**[init_analysis ~data_lim ~split_data ~tag name] initalizes an analysis with the provided [name]

- [data_lim] sets a limit on analyzed written data (-1 by default, no limit)
- [split_data] controls whether written data is split into individual bytes (off by default)
- [tag] sets the policy's tag
*)

module P : Policy.Sig with type p = A.t
