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

type unop = Restrict of int * int | Extend of int | Strlen of Z.t
type bnop = Add | Sub | Mul | Max | Min | Dist

type t = 
    | Const of Z.t * int
    | Var of Storage.t
    | Unop of unop * t
    | Bnop of bnop * t * t

let pp_unop = function
    | Restrict(b, e) -> Printf.sprintf "%d->%d" b e
    | Extend(s) -> Printf.sprintf "ext(%d)" s
    | Strlen(s) -> Printf.sprintf "strlen(%d)" @@ Z.to_int s

let parse_unop s =
    let restrictexp = Str.regexp {|\([0-9]+\)->\([0-9]+\)|} in
    let extexp = Str.regexp {|ext(\([0-9]+\))|} in
    let strlenexp = Str.regexp {|strlen(\([0-9]+\))|} in
    if Str.string_match restrictexp s 0
    then Restrict(int_of_string @@ Str.matched_group 1 s, int_of_string @@ Str.matched_group 2 s)
    else if Str.string_match extexp s 0
    then Extend(int_of_string @@ Str.matched_group 1 s)
    else if Str.string_match strlenexp s 0
    then Strlen(Z.of_string @@ Str.matched_group 1 s)
    else raise (Failure(Printf.sprintf "unknown expr unop <%s>" s))
    

let pp_bnop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Max -> "max"
    | Min -> "min"
    | Dist -> "dist"

let parse_bnop = function
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "max" -> Max
    | "min" -> Min
    | "dist" -> Dist
    | s -> raise (Failure(Printf.sprintf "unknown expr bnop <%s>" s))

let pp expr =
    let rec pp_expr = function
        | Const(z, size) -> Printf.sprintf "%s[%d]" (Z.format "%#x" z) size
        | Var(sto) -> Storage.pp sto
        | Unop(op, e) -> Printf.sprintf "(%s %s)" (pp_unop op) @@ pp_expr e
        | Bnop(op, a, b) -> Printf.sprintf "(%s %s %s)" (pp_expr a) (pp_bnop op) @@ pp_expr b
    in
    pp_expr expr

let rec size = function
    | Const(z, size) -> size
    | Var(sto) -> Storage.size sto
    | Unop(Restrict(b, e), a) ->
    (
        let sizea = size a in
        assert (e < sizea);
        e - b + 1
    )
    | Unop(Extend(s), a) ->
    (
        let sizea = size a in
        assert (sizea <= s);
        s
    )
    | Unop(Strlen(_), _) -> 8
    | Bnop(_, a, b) ->
    (
        let sizea = size a in
        let sizeb = size b in
        assert (sizea = sizeb);
        sizea
    )

let rec is_const = function
    | Const(_, _) -> true
    | Var(_) -> false
    | Unop(_, e) -> is_const e
    | Bnop(_, a, b) -> (is_const a) && (is_const b)

let get_stos expr =
    let rec aux res = function
        | Const(_) -> res
        | Var(sto) -> sto::res
        | Unop(_, e) -> aux res e
        | Bnop(_, a, b) -> aux (aux res a) b
    in
    List.sort_uniq Storage.compare @@ aux [] expr

let rec simplify expr =
    match expr with
    | Const(_)
    | Var(_) -> expr
    | Unop(op, e) ->
    (
        let ne = simplify e in
        match op, ne with
        | Restrict(b, e), Var(Storage.Memory(addr, size)) -> Var(Memory(Address.add_int addr b , e - b + 1))
        | Restrict(b, e), Const(value, size) -> Const(Z.extract value (b * 8) ((e - b) * 8), size)
        | Extend(s), Var(Memory(addr, _)) -> Var(Memory(addr, s))
        | Extend(s), Const(value, _) -> Const(value, s)
        | Strlen(s), _ -> Const(s, 8)
        | _ -> Unop(op, ne)
    )
    | Bnop(op, a, b) ->
    (
        let na = simplify a in
        let nb = simplify b in
        match op, na, nb with
        | Add, Const(va, size), Const(vb, _) -> Const(Z.add va vb, size)
        | Sub, Const(va, size), Const(vb, _) -> Const(Z.sub va vb, size)
        | Mul, Const(va, size), Const(vb, _) -> Const(Z.mul va vb, size)
        | Min, Const(va, size), Const(vb, _) -> Const(min va vb, size)
        | Max, Const(va, size), Const(vb, _) -> Const(max va vb, size)
        | Dist, Const(va, size), Const(vb, _) -> Const(Z.abs @@ Z.sub va vb, size)
        | _ -> Bnop(op, na, nb)
    )
