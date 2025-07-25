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

(**taint analysis utilities*)

module type TaintTag = 
    sig
        type t

        val from_source : Source.t -> (t * Storage.t) list
        val pp : t option -> string
        val equals : t option -> t option -> bool
        val to_generic : t option -> Result.generic
    end

type 'a multi = Single of 'a | Mixed of 'a list

module type MultiTagSig =
    sig
        type tt

        include TaintTag with type t = tt multi

        val to_list : t option -> tt list
        val of_list : tt list -> t option
        val merge : ?tt_merge:(tt -> tt -> tt option) -> t option -> t option -> t option
    end

module MakeMultiTag (T : TaintTag) : MultiTagSig with type tt = T.t

module type TaintBank =
    sig
        type tt

        include Analysis.Bank with type Res.value = (Storage.t * tt option list option) list

        val taint : Storage.t -> tt option -> t -> t
        (**[taint sto tt b] taints [sto] with a taint tag [tt] (removes taint if [tt = None]) in b*)

        val taint_byte : Storage.t -> tt option list option -> t -> t
        (**[taint_byte sto l b] taints the bytes of [sto] with the taint tags in [l], each corresponding to a single byte, in [b]*)

        val has_taint : Storage.t -> t -> bool

        val has_single_taint : Storage.t -> t -> bool
        (**checks if a storage has a single kind of taint*)

        val get_taint : Storage.t -> t -> tt option list option

        val get_single_taint : Storage.t -> t -> tt option
        (**get a storage's taint if it only has one kind*)

        val iter_reg : (string -> tt option list -> unit) -> t -> unit
        val iter_mem : (Address.t -> tt -> unit) -> t -> unit
    end

module MakeBank (T : TaintTag) : TaintBank with type tt = T.t
