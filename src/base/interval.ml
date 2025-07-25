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
    {
        lo : Z.t;
        hi : Z.t
    }

let create ~lo ~hi =
    if Z.lt lo hi
    then {lo; hi}
    else raise (Failure (Printf.sprintf "invalid interval bounds <%s> and <%s>" (Z.format "%#x" lo) (Z.format "%#x" hi)))

let pp i =
    "[" ^
    (Z.format "%#x" i.lo) ^
    "; " ^
    (Z.format "%#x" @@ Z.sub i.hi Z.one) ^
    "]"

let halve i =
    let hi = Z.add i.lo @@ Z.div (Z.sub i.hi i.lo) @@ Z.of_int 2 in
    if hi = i.lo 
    then [i]
    else
    (
        let lo = hi in
        [{i with hi}; {i with lo}]
    )

let split pivot i =
    if (Z.lt pivot i.lo) || (Z.geq pivot i.hi)
    then raise (Failure (Printf.sprintf "cannot split interval %s around <%s>" (pp i) (Z.format "%#x" pivot)))
    else 
    (
        match pivot = i.lo, pivot = Z.sub i.hi Z.one with
        | true, true -> []
        | true, false -> [create ~lo:(Z.add i.lo Z.one) ~hi:i.hi]
        | false, true -> [create ~lo:i.lo ~hi:pivot]
        | false, false -> [create ~lo:i.lo ~hi:pivot; create ~lo:(Z.add pivot Z.one) ~hi:i.hi]
    )

let compare i j =
    if Z.leq i.hi j.lo
    then -1
    else if Z.geq i.lo j.hi
    then 1
    else 0

let equals i j =
    i.lo = j.lo && i.hi = j.hi

let dist i j =
    if Z.leq i.hi j.lo
    then Z.sub j.lo i.hi
    else if Z.geq i.lo j.hi
    then Z.sub i.lo j.hi
    else Z.zero

let union i j =
    let aux i j =
        if i.hi = j.lo
        then {i with hi = j.hi}
        else raise (Failure "interval union failed: disjointed intervals")
    in
    match compare i j with
    | 0 ->
    (
        let lo = min i.lo j.lo in
        let hi = max i.hi j.hi in
        {lo; hi}
    )
    | -1 -> aux i j
    | 1 -> aux j i
    | _ -> assert false

let card i =
    Z.sub i.hi i.lo

let contains i n =
    (Z.geq n i.lo) && Z.lt n i.hi

let rec pick_random i =
    let card = card i in
    let nbits = Z.log2up card in
    let rec aux off =
        if off + 30 >= nbits
        then 
        (
            let top = Z.extract card off 30 in
            if top = Z.zero
            then Z.shift_left (Z.of_int @@ Random.bits ()) off
            else Z.shift_left (Z.of_int @@ Random.int @@ Z.to_int top) off
        )
        else
        (
            let r = Z.shift_left (Z.of_int @@ Random.bits ()) off in
            Z.add r @@ aux (off + 30)
        )
    in
    let r = Z.add i.lo @@ aux 0 in
    if contains i r
    then r
    else pick_random i

let overlap i j =
    (compare i j) = 0

let subset i j =
    (Z.leq j.lo i.lo) && (Z.geq j.hi i.hi)

let inter i j =
    let lo = max i.lo j.lo in
    let hi = min i.hi j.hi in
    create ~lo ~hi

let sub i j =
    let inter = inter i j in
    let prev =
        if i.lo < inter.lo
        then
        (
            let lo = i.lo in
            let hi = inter.lo in
            Some(create ~lo ~hi)
        )
        else None
    in
    let next = 
        if i.hi > inter.hi
        then
        (
            let lo = inter.hi in
            let hi = i.hi in
            Some(create ~lo ~hi)
        )
        else None
    in
    prev, next
