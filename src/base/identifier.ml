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
        name : string;
        id : int;
        tag : string option
    }

let cnt = ref 0

let create ?(cnt = cnt) ?(tag = None) name =
    let id = !cnt in
    cnt := !cnt + 1;
    {name; id; tag}

let get_name i =
    i.name

let get_tag i =
    i.tag

let compare a b =
    a.id - b.id

let equals a b =
    a.id = b.id

let pp i =
    let tag =
        match i.tag with
        | Some(tag) -> tag ^ ":"
        | _ -> ""
    in
    Printf.sprintf "%s <%s%d>" i.name tag i.id

let pp_basic i =
    let tag =
        match i.tag with
        | Some(tag) -> tag ^ "_"
        | _ -> ""
    in
    Printf.sprintf "%s_%s%d" i.name tag i.id
