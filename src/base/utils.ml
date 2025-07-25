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

let pattern_list l =
    let exp = Str.regexp {|<\([^>;]*\)>|} in
    Str.regexp
        @@ List.fold_left
            (
                fun res e ->
                (
                    if e = ""
                    then res
                    else 
                    (
                        let pattern = 
                            if Str.string_match exp e 0
                            then Str.matched_group 1 e
                            else Str.quote e
                        in
                        res ^ {|\||} ^ pattern
                    )
                )
            )
            "$" l

let pp_list ?(sep = ", ") pp = function
    | h::t -> List.fold_left (fun res e -> res ^ sep ^ (pp e)) (pp h) t
    | _ -> ""

let z_of_bytes blist =
    let rec aux base l =
        match l with
        | h::t -> Z.add (Z.mul h base) (aux (Z.mul base (Z.of_int 256)) t)
        | [] -> Z.zero
    in
    match Binsec.Kernel_options.Machine.endianness () with
    | Binsec.Machine.LittleEndian -> aux Z.one blist
    | Binsec.Machine.BigEndian -> aux Z.one (List.rev blist)

let bytes_of_z z size =
    let rec aux res n =
        if n = size
        then res
        else
        (
            let byte = Z.extract z (n * 8) 8 in
            aux (byte::res) (n + 1)
        )
    in
    let res = aux [] 0 in
    match Binsec.Kernel_options.Machine.endianness () with
    | Binsec.Machine.LittleEndian -> List.rev res
    | Binsec.Machine.BigEndian -> res

let z_to_int z =
    if Z.gt z @@ Z.of_int @@ Int.max_int
    then Int.max_int
    else Z.to_int z

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)
module IdMap = Map.Make (Identifier)
module ZMap = Map.Make (Z)
