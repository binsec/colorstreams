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

type t =
    | Memory of Address.t * int
    | Register of string * int
    | Custom of string * int * Z.t

let size s =
    match s with
    | Memory(_, size)
    | Register(_, size)
    | Custom(_, size, _) -> size

let pp s =
    match s with
    | Custom(name, size, value) -> Printf.sprintf "CUSTOM<%s>[%d] := %s" name size @@ Address.to_string_hex value
    | Register(reg, size) -> Printf.sprintf "REG<%s>[%d]" reg size
    | Memory(addr, size) -> Printf.sprintf "MEM<%s>[%d]" (Address.to_string_hex addr) size

let compare s1 s2 =
    let vtype = function
        | Register(_) -> 0
        | Memory(_) -> 1
        | Custom(_) -> 2
    in
    match s1, s2 with
    | Memory(addr1, size1), Memory(addr2, size2) ->
    (
        let cmp = Z.compare addr1 addr2 in
        if cmp = 0 then size1 - size2 else cmp
    )
    | Register(name1, _), Register(name2, _)
    | Custom(name1, _, _), Custom(name2, _, _) -> String.compare name1 name2
    | _ -> (vtype s1) - (vtype s2)

let value_interval sto =
    let lo = Z.zero in
    let hi = Z.shift_left Z.one ((size sto) * 8) in
    Interval.create ~lo ~hi

let memory_interval = function
    | Memory(addr, size) -> Interval.create ~lo:addr ~hi:(Address.add_int addr size)
    | _ -> assert false
