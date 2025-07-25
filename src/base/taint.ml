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

module type TaintTag = 
    sig
        type t

        val from_source : Source.t -> (t * Storage.t) list
        val pp : t option -> string
        val equals : t option -> t option -> bool
        val to_generic : t option -> Result.generic
    end

type 'a multi = Single of 'a | Mixed of 'a list

module type MultiTagSig =
    sig
        type tt

        include TaintTag with type t = tt multi

        val to_list : t option -> tt list
        val of_list : tt list -> t option
        val merge : ?tt_merge:(tt -> tt -> tt option) -> t option -> t option -> t option
    end

module MakeMultiTag (T : TaintTag) =
    struct
        type tt = T.t

        module ResValT = 
            struct
                type t = T.t

                let to_generic r =
                    T.to_generic (Some(r))
            end

        module ResT = Result.Make(ResValT)

        type t = T.t multi

        let from_source src =
            List.map (fun (tt, sto) -> (Single(tt)), sto) @@ T.from_source src

        let pp = function
            | Some(Single(tt)) -> T.pp (Some(tt))
            | Some(Mixed(l)) -> Printf.sprintf "<%s>" @@ Utils.pp_list (fun tt -> T.pp (Some(tt))) l
            | None -> "-"

        let equals tt1 tt2 =
            match tt1, tt2 with
            | (Some(Single(tt1))), Some(Single(tt2)) -> T.equals (Some(tt1)) (Some(tt2))
            | (Some(Mixed(l1))), Some(Mixed(l2)) -> ((List.length l1) = List.length l2)
                && List.fold_left2 (fun res tt1 tt2 -> res && T.equals (Some(tt1)) (Some(tt2))) true l1 l2
            | _ -> false

        let to_generic = function
            | Some(Single(tt)) -> T.to_generic (Some(tt))
            | Some(Mixed(l)) -> Result.Res(List.mapi (fun i tt -> ResT.export @@ ResT.create ~label:(string_of_int i) tt) l)
            | _ -> Result.Str("-")

        let to_list = function
            | Some(Single(tt)) -> [tt]
            | Some(Mixed(l)) -> l
            | _ -> []

        let of_list = function
            | [] -> None
            | [tt] -> Some(Single(tt))
            | l -> Some(Mixed(l))

        let default_tt_merge tt1 tt2 =
            if T.equals (Some(tt1)) (Some(tt2))
            then Some(tt1)
            else None
                
        let merge ?(tt_merge = default_tt_merge) tt1 tt2 =
            let do_merge res tt =
                let res, merged = List.fold_right 
                    (
                        fun e (res, merged) ->
                            if merged 
                            then e::res, merged
                            else
                            (
                                match tt_merge e tt with
                                | Some(tt) -> tt::res, true
                                | None -> e::res, merged
                            )
                    )
                    res ([], false)
                in
                if merged
                then res
                else tt::res
            in
            match tt1, tt2 with
            | (Some(tt1)), (Some(tt2)) -> 
            (
                let res =
                    match tt1, tt2 with
                    | (Mixed(l1)), Mixed(l2) -> List.fold_left do_merge l1 l2
                    | (Single(tt)), Mixed(l)
                    | (Mixed(l)), Single(tt) -> do_merge l tt
                    | (Single(tt1)), Single(tt2) -> do_merge [tt1] tt2
                in
                match res with
                | [] -> assert false
                | [tt] -> Some(Single(tt))
                | _ -> Some(Mixed(res))
            )
            | (Some(_)), _ -> tt1
            | _, Some(_) -> tt2
            | _ -> None
    end
    
module type TaintBank =
    sig
        type tt

        include Analysis.Bank with type Res.value = (Storage.t * tt option list option) list

        val taint : Storage.t -> tt option -> t -> t
        val taint_byte : Storage.t -> tt option list option -> t -> t
        val has_taint : Storage.t -> t -> bool
        val has_single_taint : Storage.t -> t -> bool
        val get_taint : Storage.t -> t -> tt option list option
        val get_single_taint : Storage.t -> t -> tt option

        val iter_reg : (string -> tt option list -> unit) -> t -> unit
        val iter_mem : (Address.t -> tt -> unit) -> t -> unit
    end

module MakeBank (T : TaintTag) =
    struct
        type tt = T.t

        module RegBank = Map.Make(String)
        module MemBank = Map.Make(Address)

        module ResVal =
            struct
                type t = (Storage.t * T.t option list option) list

                let to_generic r =
                    let aux = function
                        | sto, Some(l) -> Result.mk_list ~label:(Storage.pp sto) @@ List.map T.to_generic l
                        | sto, None -> Result.mk_list ~label:(Storage.pp sto) @@ List.init (Storage.size sto) (fun _ -> T.to_generic None)
                    in
                    Result.Res(List.map aux r)
            end

        module Res = Result.Make(ResVal)

        type t = 
        {
            regbank : T.t option list RegBank.t;
            membank : T.t MemBank.t;
            reg_cnt : Stats.counter;
            mem_cnt : Stats.counter
        }

        let create () =
            {
                regbank = RegBank.empty;
                membank = MemBank.empty;
                reg_cnt = new Stats.counter "reg" "Tainted registers" 0;
                mem_cnt = new Stats.counter "mem" "Tainted memory bytes" 0
            }

        let add_stats_to_bundle bnk b =
            b
                |> Stats.Bundle.add_stat (bnk.reg_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (bnk.mem_cnt :> Stats.stat)

        let get_register_taint reg b =
            try
                Some(RegBank.find reg b.regbank)
            with Not_found -> None

        let taint_register reg tt b =
            let was = 
                match get_register_taint reg b with
                | Some(_) -> true
                | _ -> false
            in
            let regbank =
                match tt with
                | Some(tt) -> 
                (
                    if not was then b.reg_cnt#incr;
                    RegBank.add reg tt b.regbank
                )
                | None -> 
                (
                    if was then b.reg_cnt#decr;
                    RegBank.remove reg b.regbank
                )
            in
            {b with regbank}

        let get_memory_taint addr size b =
            let some = ref false in
            let rec aux addr size =
                if size > 0
                then
                (
                    let tt =
                        try
                            let tt = MemBank.find addr b.membank in
                            some := true;
                            Some(tt)
                        with Not_found -> None
                    in
                    tt::(aux (Address.incr addr) (size - 1))
                )
                else []
            in
            let res = aux addr size in
            if !some
            then Some(res)
            else None

        let taint_memory addr size tt b =
            let was =
                match get_memory_taint addr size b with
                | Some(tt) -> List.map (function Some(_) -> true | None -> false) tt
                | None -> List.init size (fun _ -> false)
            in
            let tt =
                match tt with
                | Some(l) -> l
                | None -> List.init size (fun _ -> None)
            in
            let rec aux addr size tt was =
                if size > 0
                then
                (
                    match tt, was with
                    | (Some(tt))::ttt, was::wast ->
                    (
                        if not was then b.mem_cnt#incr;
                        MemBank.add addr tt (aux (Address.incr addr) (size - 1) ttt wast)
                    )
                    | None::ttt, was::wast -> 
                    (
                        if was then b.mem_cnt#decr;
                        MemBank.remove addr (aux (Address.incr addr) (size - 1) ttt wast)
                    )
                    | _ -> assert false
                )
                else b.membank
            in
            {b with membank = aux addr size tt was}

        let taint sto tt b =
            let tt = 
                match tt with
                | Some(tt) -> Some(List.init (Storage.size sto) (fun _ -> Some(tt)))
                | None -> None
            in
            match sto with
            | Storage.Custom(name, _, _)
            | Storage.Register(name, _) -> taint_register name tt b
            | Storage.Memory(addr, size) -> taint_memory addr size tt b

        let taint_byte sto tt b =
            let tt =
                match tt with
                | Some(l) -> 
                (
                    assert ((List.length l) = Storage.size sto);
                    match List.find_map (fun tt -> tt) l with
                    | Some(_) -> Some(l)
                    | None -> None
                )
                | None -> None
            in
            match sto with
            | Storage.Custom(name, _, _)
            | Storage.Register(name, _) -> taint_register name tt b
            | Storage.Memory(addr, size) -> taint_memory addr size tt b

        let get_taint sto b =
            match sto with
            | Storage.Custom(name, _, _)
            | Storage.Register(name, _) -> get_register_taint name b
            | Storage.Memory(addr, size) -> get_memory_taint addr size b

        let has_taint sto b =
            match get_taint sto b with
            | Some(_) -> true
            | _ -> false

        let get_single_taint sto b =
            match get_taint sto b with
            | Some(l) -> List.fold_left
                (
                    fun res tt -> 
                        if T.equals res tt
                        then res
                        else None
                )
                (List.hd l) @@ List.tl l
            | None -> None

        let has_single_taint sto b =
            match get_single_taint sto b with
            | Some(_) -> true
            | _ -> false

        let source src b =
            List.fold_left (fun b (tt, sto) -> taint sto (Some(tt)) b) b @@ T.from_source src

        let sink snk b =
            let res, some = List.fold_left 
                (
                    fun (res, some) sto -> 
                        match get_taint sto b with
                        | Some(t) -> (sto, Some(t))::res, true
                        | None -> (sto, None)::res, some
                ) 
                ([], false)
                @@ Expr.get_stos 
                @@ Expr.simplify snk.Sink.expr 
            in
            let stats = Some(add_stats_to_bundle b @@ Stats.Bundle.create "stats") in
            let some =
                match snk.Sink.constr with
                | Some(sto) ->
                (
                    let constr = Trace.stoval sto in
                    if Z.gt constr Z.zero
                    then some
                    else false
                )
                | _ -> some
            in
            (Res.create ~label:"Taint" ~stats res), some

        let pp_mem b =
            let cnt = ref 0 in
            let prev = ref Address.zero in
            let t = ref None in
            let aux k e res =
                if !cnt = 0
                then
                (
                    prev := k;
                    cnt := 1;
                    t := Some(e);
                    res
                )
                else
                (
                    if (Address.equal k (Address.add_int !prev !cnt)) && (T.equals (Some(e)) !t)
                    then 
                    (
                        cnt := !cnt + 1;
                        res
                    )
                    else
                    (
                        let s = res ^ (Printf.sprintf "%s[%d](%s), " (Address.to_string_hex !prev) !cnt (T.pp !t)) in
                        prev := k;
                        cnt := 1;
                        t := Some(e);
                        s
                    )
                )
            in
            let s = MemBank.fold aux b.membank "" in
            Printf.sprintf "|-> Tainted memory: %s%s"
                s
                (
                    if !cnt > 0 
                    then 
                    (
                        Printf.sprintf "%s[%d](%s)" (Address.to_string_hex !prev) !cnt (T.pp !t)
                    )
                    else ""
                )

        let pp_reg b =
            RegBank.fold (fun k e res -> res ^ (Printf.sprintf "%s (%s), " k (Utils.pp_list T.pp e))) b.regbank "|-> Tainted registers: "

        let pp b =
            Printf.sprintf "%s\n%s" (pp_reg b) (pp_mem b)

        let iter_reg f b =
            RegBank.iter f b.regbank

        let iter_mem f b =
            MemBank.iter f b.membank
    end
