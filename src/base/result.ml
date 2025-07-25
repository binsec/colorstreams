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

type 'a bundle =
    {
        label : string;
        stats : Stats.Bundle.t option;
        result : 'a;
        custom_printer : string list Lazy.t option
    }

type generic = Bool of bool | Int of Z.t | Flt of float | Itv of Interval.t * Z.t * Z.t | Str of string | Sto of Storage.t | StoExpr of Expr.t | Lst of generic list | Res of generic bundle list

type t = generic bundle

let res_cnt = ref 0

let create ?(label = "") ?(stats = None) result =
    let label =
        if label = ""
        then 
        (
            let label = Printf.sprintf "result_%d" !res_cnt in
            res_cnt := !res_cnt + 1;
            label
        )
        else label
    in
    {label; stats; result; custom_printer = None}
    
let mk_bool ?(label = "") ?(stats = None) b =
    create ~label ~stats (Bool(b))

let mk_int ?(label = "") ?(stats = None) i =
    create ~label ~stats (Int(i))

let mk_float ?(label = "") ?(stats = None) f =
    create ~label ~stats (Flt(f))

let mk_interval ?(label = "") ?(stats = None) ?(card = None) itv =
    match card with
    | Some(lo, hi) -> create ~label ~stats (Itv(itv, lo, hi))
    | None -> 
    (
        let card = Interval.card itv in
        create ~label ~stats (Itv(itv, card, card))
    )

let mk_string ?(label = "") ?(stats = None) s =
    create ~label ~stats (Str(s))

let mk_storage ?(label = "") ?(stats = None) sto =
    create ~label ~stats (Sto(sto))

let mk_sto_expr ?(label = "") ?(stats = None) expr =
    create ~label ~stats (StoExpr(expr))

let mk_list ?(label = "") ?(stats = None) l =
    create ~label ~stats (Lst(l))

let mk_res_list ?(label = "") ?(stats = None) l =
    create ~label ~stats (Res(l))

let set_custom_printer custom_printer res =
    {res with custom_printer}

module StringMap = Map.Make (String)

let reslist_merge r =
    match r.result with
    | Res(l) ->
    (
        let resmap = ref StringMap.empty in
        List.iter 
            (
                fun r -> 
                (
                    let label = r.label in
                    try
                        let rref = StringMap.find label !resmap in
                        match (!rref).result, r.result with
                        | Res(l1), Res(l2) -> 
                        (
                            let stats = (!rref).stats in
                            rref := mk_res_list ~label ~stats (l1 @ l2)
                        )
                        | _, Res(l) -> 
                        (
                            let stats = r.stats in
                            rref := mk_res_list ~label ~stats (!rref::l)
                        )
                        | Res(l), _ -> 
                        (
                            let stats = (!rref).stats in
                            rref := mk_res_list ~label ~stats (l @ [r])
                        )
                        | _, _ -> rref := mk_res_list ~label [!rref; r]
                    with Not_found -> resmap := StringMap.add label (ref r) !resmap
                )
            )
            l
        ;
        let l = List.fold_left
            (
                fun res r -> 
                (
                    try
                        let rref = StringMap.find r.label !resmap in
                        resmap := StringMap.remove r.label !resmap;
                        !rref::res
                    with Not_found -> res
                )
            )
            [] l
        in
        let label = r.label in
        let stats = r.stats in
        mk_res_list ~label ~stats @@ List.rev l
    )
    | _ -> r

let add_stat s r =
    let stats =
        match r.stats with
        | Some(b) -> Some(Stats.Bundle.add_stat s b)
        | None -> Some(Stats.Bundle.add_stat s @@ Stats.Bundle.create "")
    in
    {r with stats}

let pp ?(pstats = false) r =
    let rec pp_res = function
        | Bool(b) -> if b then "True" else "False"
        | Int(i) -> Z.format "%#x" i
        | Flt(f) -> Printf.sprintf "%.3g" f
        | Itv(itv, lo, hi) -> 
        (
            if lo = hi
            then Printf.sprintf "%s (%s)" (Interval.pp itv) @@ Z.format "%#x" lo
            else Printf.sprintf "%s (%s - %s)" (Interval.pp itv) (Z.format "%#x" lo) @@ Z.format "%#x" hi
        )
        | Str(s) -> s
        | Sto(sto) -> Storage.pp sto
        | StoExpr(expr) -> Expr.pp expr
        | Lst([])
        | Res([]) -> "<>"
        | Lst(h::t) -> (List.fold_left (fun res e -> res ^ ", " ^ (pp_res e)) ("<" ^ (pp_res h)) t) ^ ">"
        | Res(h::t) -> (List.fold_left (fun res e -> res ^ ", " ^ (pp_res e.result)) ("<" ^ (pp_res h.result)) t) ^ ">"
    in
    let rec pp_ r =
        let label = r.label ^ ": " in
        let label, lst =
            let lst =
                if pstats
                then
                (
                    match r.stats with
                    | Some(stats) -> List.map (fun e -> true, e) @@ Stats.Bundle.pp_stats stats
                    | None -> []
                )
                else []
            in
            match r.custom_printer, r.result with
            | Some(printer), _ -> (true, label), List.map (fun e -> true, e) @@ Lazy.force printer
            | _, Res(l) -> (true, label), ((List.fold_right (fun e res -> (pp_ e) @ res) l []) @ lst)
            | _, r -> (true, (label ^ (pp_res r))), lst
        in
        let rec add_prefix l =
            match l with
            | (f, h)::t -> 
            (
                let last, lst = add_prefix t in
                if f
                then
                (
                    if last
                    then false, (false, ("└─" ^ h))::lst
                    else false, (false, ("├─" ^ h))::lst
                )
                else 
                (
                    if last
                    then last, (false, ("  " ^ h))::lst
                    else last, (false, ("│ " ^ h))::lst
                )
            )
            | [] -> true, []
        in
        let _, lst = add_prefix lst in
        label::lst
    in
    let lst = pp_ r in
    List.fold_left (fun res (_, e) -> res ^ "\n" ^ e) (match List.hd lst with _, e -> e) @@ List.tl lst

module type CustomVal =
    sig
        type t

        val to_generic : t -> generic
    end

type custom_result = ..
type wrapped_result = Generic of t | Custom of custom_result * t Lazy.t | Mixed of string * wrapped_result list
type Message.ext_value += Result of wrapped_result

let rec to_generic = function
    | Mixed(label, l) -> mk_res_list ~label @@ List.map to_generic l
    | Custom(_, res) -> Lazy.force res
    | Generic(res) -> res

let send ?(verbosity = 0) ~prefix r =
    let ext_value = Result(r) in
    let value = lazy (pp ~pstats:(!Message.verbosity > 0) @@ to_generic r) in
    Message.accept {verbosity; error = None; ext_value; value; prefix; handled_by = []}

module type CustomResult =
    sig
        type value
        type result = value bundle
        type custom_result += CustomResult of result

        val create : ?label:string -> ?stats:(Stats.Bundle.t option) -> value -> result
        val set_custom_printer : string list Lazy.t option -> result -> result
        val export : result -> t
        val add_stat : Stats.stat -> result -> result
        val send : ?verbosity:int -> prefix:string -> result -> unit
        val wrap : result -> wrapped_result
    end

module Make (V : CustomVal) =
    struct
        type value = V.t
        type result = value bundle
        type custom_result += CustomResult of result

        let create ?(label = "") ?(stats = None) result =
            {label; stats; result; custom_printer = None}

        let set_custom_printer custom_printer res =
            {res with custom_printer}

        let export r =
            {r with result = V.to_generic r.result}

        let add_stat = add_stat

        let send ?(verbosity = 0) ~prefix r =
            send ~verbosity ~prefix (Custom((CustomResult(r)), lazy (export r)))

        let wrap r =
            Custom((CustomResult(r)), lazy (export r))
    end

open Yojson.Safe

let rec to_json r =
    let rec result_to_json = function
        | Bool(b) -> `Bool(b)
        | Int(i) -> `String("Z:" ^ (Z.to_string i))
        | Flt(f) -> `Float(f)
        | Itv(itv, lo, hi) -> `Assoc
            [
                ("lo", result_to_json (Int(itv.Interval.lo))); 
                ("hi", result_to_json (Int(itv.Interval.hi)));
                ("card_lo", result_to_json (Int(lo)));
                ("card_hi", result_to_json (Int(hi)))
            ] 
        | Str(s) -> `String(s)
        | Sto(sto) ->
        (
            let sto, size, typ =
                match sto with
                | Storage.Custom(name, size, _) -> `String(name), `Int(size), `String("custom")
                | Storage.Register(name, size) -> `String(name), `Int(size), `String("reg")
                | Storage.Memory(addr, size) -> result_to_json (Int(addr)), `Int(size), `String("mem")
            in
            `Assoc
                [
                    ("storage", sto);
                    ("size", size);
                    ("type", typ)
                ]
        )
        | StoExpr(expr) ->
        (
            `Assoc([
                (
                    "expr",
                    (
                        match expr with
                        | Expr.Const(z, size) -> `Assoc([("size", `Int(size)); ("value", result_to_json (Int(z)))])
                        | Expr.Var(sto) -> result_to_json (Sto(sto))
                        | Expr.Unop(op, e) -> `Assoc([(Expr.pp_unop op), result_to_json (StoExpr(e))])
                        | Expr.Bnop(op, a, b) -> `Assoc([(Expr.pp_bnop op), `List([(result_to_json (StoExpr(a))); result_to_json (StoExpr(b))])])
                    )
                );
                ("size", `Int(Expr.size expr))
            ])
        )
        | Lst(l) -> `List(List.map result_to_json l)
        | Res(l) -> 
        (
            let labels = ref StringMap.empty in
            let check_label s =
                try
                    let cnt = StringMap.find s !labels in
                    cnt := !cnt + 1;
                    Printf.sprintf "%s_%d" s !cnt
                with Not_found -> 
                (
                    labels := StringMap.add s (ref 1) !labels;
                    s
                )
            in
            `Assoc(List.map (fun e -> ((check_label e.label), to_json e)) l)
        )
    in
    let res = [("result", (result_to_json r.result))] in
    `Assoc
        (
            match r.stats with
            | Some(stats) -> ("stats", Stats.Bundle.to_json stats)::res
            | None -> ("stats", `Null)::res
        )

(*TODO: implement stats*)
let rec from_json json =
    let rec aux = function
        | `Bool(b) -> Bool(b)
        | `String(s) when String.starts_with ~prefix:"Z:" s -> Int(Z.of_string @@ String.sub s 2 ((String.length s) - 2))
        | `Float(f) -> Flt(f)
        | `Assoc
            (
                [
                    ("lo", lo);
                    ("hi", hi);
                    ("card_lo", card_lo);
                    ("card_hi", card_hi)
                ]
            )
                as itv ->
        (
            match aux lo, aux hi, aux card_lo, aux card_hi with
            | Int(lo), Int(hi), Int(card_lo), Int(card_hi) -> Itv(Interval.create ~lo ~hi, card_lo, card_hi)
            | _ -> raise (Failure(Printf.sprintf "Failed to parse interval <%s> from json" @@ Yojson.Safe.to_string itv))
        )
        | `Assoc
            [
                ("storage", sto);
                ("size", `Int(size));
                ("type", `String(typ))
            ]
                as storage ->
        (
            match sto, lazy (aux sto), typ with
            | `String(name), _, "custom" -> Sto(Storage.Custom(name, size, Z.zero))
            | `String(name), _, "reg" -> Sto(Storage.Register(name, size))
            | `String(_), lazy (Int(addr)), "mem" -> Sto(Storage.Memory(addr, size))
            | _ -> raise (Failure(Printf.sprintf "Failed to parse storage <%s> from json" @@ Yojson.Safe.to_string storage))
        )
        | `Assoc
            (
                [
                    ("expr", expr);
                    ("size", _)
                ]
            )
                ->
        (
            match expr with
            | `Assoc(["size", `Int(size); "value", value]) ->
            (
                match aux value with 
                | Int(value) -> StoExpr(Expr.Const(value, size))
                | _ -> raise (Failure(Printf.sprintf "Failed to parse expr const <%s> from json" @@ Yojson.Safe.to_string expr))
            )
            | `Assoc([bnop, `List([e1; e2])]) -> 
            (
                try
                    match aux e1, aux e2 with
                    | StoExpr(e1), StoExpr(e2) -> StoExpr(Expr.Bnop(Expr.parse_bnop bnop, e1, e2))
                    | _ -> raise (Failure(Printf.sprintf "Failed to parse storage in expr <%s> from json" @@ Yojson.Safe.to_string expr))
                with e -> raise (Failure(Printf.sprintf "Failed to parse expr bnop <%s> from json (%s)" (Yojson.Safe.to_string expr) @@ Printexc.to_string e))
            )
            | `Assoc([unop, e]) -> 
            (
                try
                    match aux e with
                    | StoExpr(e) -> StoExpr(Expr.Unop(Expr.parse_unop unop, e))
                    | _ -> raise (Failure(Printf.sprintf "Failed to parse storage in expr <%s> from json" @@ Yojson.Safe.to_string expr))
                with e -> raise (Failure(Printf.sprintf "Failed to parse expr unop <%s> from json (%s)" (Yojson.Safe.to_string expr) @@ Printexc.to_string e))
            )
            | expr ->
            (
                match aux expr with
                | Sto(sto) -> StoExpr(Expr.Var(sto))
                | _ -> raise (Failure(Printf.sprintf "Failed to parse expr var <%s> from json" @@ Yojson.Safe.to_string expr))
            )
        )
        | `List(l) -> Lst(List.map aux l)
        | `Assoc(l) -> Res(List.map (fun (label, res) -> create ~label @@ from_json res) l)
        | `String(s) -> Str(s)
        | j -> raise (Failure(Printf.sprintf "Failed to parse result <%s> from json" @@ Yojson.Safe.to_string j))
    in
    match json with
    | `Assoc(["stats", _; "result", r]) -> aux r
    | _ -> raise (Failure(Printf.sprintf "Failed to parse result <%s> from json" @@ Yojson.Safe.to_string json))

class mh () =
    object (self)
        inherit Message.handler () as super

        val mutable results = StringMap.empty

        method handle msg =
            let _ =
                let store res =
                    let label = 
                        match msg.handled_by with
                        | h::t -> List.fold_left (fun res e -> res ^ ":" ^ (Identifier.pp e)) (Identifier.pp h) t
                        | _ -> "anon"
                    in
                    try
                        let l = StringMap.find label results in
                        results <- StringMap.add label (res::l) results
                    with Not_found -> results <- StringMap.add label [res] results
                in
                match msg.Message.ext_value with
                | Result(res) -> store @@ to_generic res
                | _ -> ()
            in
            super#handle msg

        method to_json =
            let results = StringMap.fold (fun k e res -> (mk_res_list ~label:k e)::res) results [] in
            ((`Assoc(List.map (fun r -> (r.label, to_json r)) results)) : Yojson.Safe.t)
    end

let load_from_file file =
    match Yojson.Safe.from_file @@ File.get_path file with
    | `Assoc(["results", `Assoc(l); _]) -> create ~label:"results" (Res(List.map (fun (label, res) -> create ~label @@ from_json res) l))
    | _ -> raise (Failure(Printf.sprintf "Failed to parse results from json file <%s>" @@ File.get_path file))
