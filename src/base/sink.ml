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
        expr : Expr.t;
        fname : string;
        kind : kind;
        more : string;
        constr : Storage.t option
    }

let pp snk =
    Printf.sprintf "%s <%s> (%s)"
        (Identifier.pp snk.name)
        (Expr.pp snk.expr)
        (
            (
                match snk.kind with
                | Custom -> "custom"
                | Auto(_, s) -> "auto " ^ s
            ) ^
            (
                if snk.more = ""
                then ""
                else ", " ^ snk.more
            )
        )

let cnt = ref 0

let create ?(dummy = false) ?(fname = "") ?(kind = Custom) ?(more = "") ?(constr = None) name expr =
    let cnt = if dummy then ref 0 else cnt in
    let name = Identifier.create ~cnt name in
    {name; expr; fname; kind; more; constr}

let sinkex = Str.regexp {|Sink(\([A-Za-z0-9]+\), \([0-9]+\), \([^,]+\), \(\([A-Za-z0-9][A-Za-z0-9], \)*[A-Za-z0-9][A-Za-z0-9]\)\(, \(0x[A-Za-z0-9]+\), \([0-9]+\), \([^)]+\)\)?)|}

let parse line =
    try
        if Str.string_match sinkex line 0
        then
        (
            try
                let address =
                    Str.matched_group 1 line
                        |> Address.of_string
                in
                let size =
                    Str.matched_group 2 line
                        |> int_of_string
                in
                let name = Str.matched_group 3 line in
                (*let value = Str.matched_group 4 line in*)
                let constr =
                    try
                        let address = Address.of_string @@ Str.matched_group 7 line in
                        let size = int_of_string @@ Str.matched_group 8 line in
                        (*let value =
                            let s = Str.matched_group 9 line in
                            let vbytes = Str.split (Str.regexp {|, |}) s in
                            List.rev @@ List.fold_left (fun res e -> (Z.of_string ("0x" ^ e))::res) [] vbytes
                        in*)
                        Some(Storage.Memory(address, size))
                    with _ -> None
                in
                (*let value =
                    let vbytes = Str.split (Str.regexp {|, |}) value in
                    List.fold_left (fun res e -> (Z.of_string ("0x" ^ e))::res) [] vbytes
                        |> List.rev
                in*)
                let storage = Storage.Memory(address, size) in
                let expr = Expr.Var(storage) in
                create ~fname:"" ~kind:Custom ~more:"" ~constr name expr
            with e -> raise (Failure (Printf.sprintf "could not parse arguments (%s)" @@ Printexc.to_string e))
        )
        else raise (Failure "expression does not match")
    with e -> raise (Failure (Printf.sprintf "could not parse sink in <%s> (%s)" line @@ Printexc.to_string e))

let alert snk =
    "!!!" ^
    (
        if snk.fname = "" 
        then ""
        else snk.fname ^ ": "
    ) ^
    "taint found in sink <" ^
    (Identifier.pp snk.name) ^
    ">" ^
    (
        if snk.more = ""
        then ""
        else " (" ^ snk.more ^ ")"
    ) ^
    "!!!"

let has_constr snk =
    match snk.constr with
    | Some(_) -> true
    | None -> false

let compare s1 s2 =
    Identifier.compare s1.name s2.name

let equals s1 s2 =
    Identifier.equals s1.name s2.name
