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

module CallConv =
    struct
        type t =
            {
                iargs : Storage.t list;
                iret : Storage.t list
            }

        let sysV = 
            let iargs = 
                [
                    (Storage.Register("rdi", 8));
                    (Storage.Register("rsi", 8));
                    (Storage.Register("rdx", 8));
                    (Storage.Register("rcx", 8));
                    (Storage.Register("r8", 8));
                    (Storage.Register("r9", 8))
                ]
            in
            let iret =
                [
                    (Storage.Register("rax", 8));
                    (Storage.Register("rdx", 8))
                ]
            in
            {iargs; iret}

        let get_reg s n l =
            let rec aux i = function
                | h::t -> if i = 0 then h else aux (i - 1) t
                | _ -> raise (Failure(Printf.sprintf "invalid function %s register number <%d>" s n))
            in
            aux n l

        let get_iarg n c =
            get_reg "integer argument" n c.iargs

        let get_iret n c =
            get_reg "integer return" n c.iret
    end

module CallId =
    struct
        type t =
            {
                site_id : int;
                stack_id : int
            }

        let create site_id stack_id =
            {site_id; stack_id}

        let compare id1 id2 =
            if id1.stack_id = id2.stack_id
            then id1.site_id - id2.site_id
            else id2.stack_id - id1.stack_id

        let pp call_id =
            Printf.sprintf "site <%d, %d>" call_id.site_id call_id.stack_id
    end

module IdMap = Map.Make(CallId)

type func = 
    {
        fname : string;
        cconv : CallConv.t;
        call_id : CallId.t option
    }

type state = (CallId.t * func list) list

let empty_state = [CallId.create (-1) (-1), []]

type t = Call of CallId.t | Entry of func | Exit of func | CallRet of CallId.t * func list

let get_fname = function
    | Entry(func)
    | Exit(func) -> Some(func.fname)
    | _ -> None

let funexp = Str.regexp {|\(Entering\|Exiting\) \([^(]*\)\((\([^)]*\))\)?|}
let callexp = Str.regexp {|\(Call\|CallRet\)(\([0-9]+\),\([0-9]+\))|}

let pp = function
    | Call(call_id) -> "Call at " ^ (CallId.pp call_id)
    | Entry(func) -> "Entering " ^ func.fname ^ 
        (
            match func.call_id with
            | Some(call_id) -> " from " ^ (CallId.pp call_id)
            | _ -> ""
        )
    | Exit(func) -> "Exiting " ^ func.fname
    | CallRet(call_id, []) -> "Returned at " ^ (CallId.pp call_id)
    | CallRet(call_id, funcs) ->
    (
        let pp_func res e =
            res ^ ", " ^ e.fname
        in
        "Returned from call(s) to <" ^ (List.fold_left pp_func ((List.hd funcs).fname) @@ List.tl funcs) ^ "> at " ^ (CallId.pp call_id)
    )

let parse line state =
    if Str.string_match funexp line 0
    then
    (
        let fname = Str.matched_group 2 line in
        match Str.matched_group 1 line with
        | "Entering" ->
        (
            match state with
            | (call_id, l)::t ->
            (
                let func = {fname; cconv = CallConv.sysV; call_id = Some(call_id)} in
                (Entry(func)), (call_id, func::l)::t
            )
            | [] -> (Entry({fname; cconv = CallConv.sysV; call_id = None})), []
        )
        | "Exiting" -> (Exit({fname; cconv = CallConv.sysV; call_id = None})), state
        | _ -> assert false
    )
    else if Str.string_match callexp line 0
    then
    (
        let site_id = int_of_string @@ Str.matched_group 2 line in
        let stack_id = int_of_string @@ Str.matched_group 3 line in
        let call_id = CallId.create site_id stack_id in
        match Str.matched_group 1 line with
        | "Call" -> (Call(call_id)), (call_id, [])::state
        | "CallRet" ->
        (
            let rec aux = function
                | (call_id_, funcs)::t ->
                (
                    if (CallId.compare call_id call_id_) = 0
                    then (CallRet(call_id, funcs)), t
                    else aux t
                )
                | [] -> (CallRet(call_id, [])), []
            in
            aux state
        )
        | _ -> assert false
    )
    else assert false
    
let get_iarg n func =
    try
        CallConv.get_iarg n func.cconv
    with Failure(msg) -> raise (Failure(Printf.sprintf "%s: %s" func.fname msg))

let get_iret n func =
    try
        CallConv.get_iret n func.cconv
    with Failure(msg) -> raise (Failure(Printf.sprintf "%s: %s" func.fname msg))

let get_toplevel = function
    | (_, {fname; _}::_)::_ -> fname
    | _ -> "???"
