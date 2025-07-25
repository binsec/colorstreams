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

type t = Interval.t list

let empty = []

let merge a b =
    let rec aux a b c =
        let push itv =
            match c with
            | h::t ->
            (
                try
                    (Interval.union itv h)::t
                with _ -> itv::c
            )
            | [] -> [itv]
        in
        match a, b with
        | ha::ta, hb::tb ->
        (
            match Interval.compare ha hb with
            | 0 -> aux ta tb @@ push @@ Interval.union ha hb
            | 1 -> aux a tb @@ push hb
            | -1 -> aux ta b @@ push ha
            | _ -> assert false
        )
        | ha::ta, [] -> (List.rev @@ push ha) @ ta
        | [], hb::tb -> (List.rev @@ push hb) @ tb
        | [], [] -> List.rev c
    in
    aux a b []

let insert_interval itv c =
    let rec insert itv res = function
        | h::t ->
        (
            try
                let union = Interval.union itv h in
                insert union res t
            with _ ->
            (
                if (Interval.compare itv h) < 0
                then (List.rev res) @ itv::h::t
                else insert itv (h::res) t
            )
        )
        | [] -> List.rev (itv::res)
    in
    insert itv [] c
    
let insert n c =
    insert_interval (Interval.create ~lo:n ~hi:(Z.add n Z.one)) c

let single n =
    insert n empty

let interval itv =
    insert_interval itv empty

let contains n c =
    try
        ignore @@ List.find (fun e -> Interval.contains e n) c;
        true
    with Not_found -> false

let pp c =
    let pp itv =
        if Interval.card itv = Z.one
        then Z.to_string itv.Interval.lo
        else Printf.sprintf "%s - %s" (Z.to_string itv.Interval.lo) @@ Z.to_string @@ Z.sub itv.Interval.hi Z.one
    in
    Utils.pp_list pp c

let equals c1 c2 =
    if (List.length c1) = List.length c2
    then 
    (
        try
            let eq = List.map2 Interval.equals c1 c2 in
            List.find (fun e -> not e) eq
        with Not_found -> true
    )
    else false
