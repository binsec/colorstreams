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

type ext_kind = ..
type kind = Custom | Auto of ext_kind * string

type t =
    {
        name : Identifier.t;
        storage : Storage.t;
        kind : kind;
        desc : string list;
    }

let cnt = ref 0

let create ?(name = "source") ?(kind = Custom) storage desc =
    let name = Identifier.create ~cnt name in
    {name; storage; kind; desc}

let sourcex = Str.regexp {|Source(\([A-Za-z0-9]+\), \([0-9]+\), \([^)]+\))|}

let parse line =
    try
        if (Str.string_match sourcex line 0)
        then
        (
            try
                let address = Str.matched_group 1 line |> Address.of_string in
                let size = Str.matched_group 2 line |> int_of_string in
                let storage = Storage.Memory (address, size) in
                let desc, name = 
                    let desc = Str.matched_group 3 line in
                    match String.split_on_char ':' desc with
                    | [msg; desc] -> desc, msg
                    | _ -> desc, "source"
                in
                let desc = String.split_on_char '/' desc in
                create ~name ~kind:Custom storage desc
            with e -> raise (Failure (Printf.sprintf "could not parse arguments (%s)" @@ Printexc.to_string e))
        )
        else raise (Failure "expression does not match")
    with e -> raise (Failure (Printf.sprintf "could not parse source in <%s> (%s)" line @@ Printexc.to_string e))

let pp src =
    Printf.sprintf "%s <%s>: %s (%s)" (Identifier.pp src.name) (Storage.pp src.storage) (Utils.pp_list (fun e -> e) src.desc)
        (
            match src.kind with
            | Custom -> "custom"
            | Auto(_, s) -> "auto " ^ s
        )
