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

type action = Propagate | Fallback | Erase

module type TBSig =
    sig
        include Taint.TaintBank

        val direct : tt option -> tt option
        val indirect : tt option -> tt option
        val control : tt option -> tt option

        val unary_taint : Binsec.Dba.Expr.t -> Binsec.Dba.Unary_op.t -> Binsec.Dba.Expr.t -> tt option list -> tt option list
        val binary_taint : Binsec.Dba.Expr.t -> Binsec.Dba.Binary_op.t -> Binsec.Dba.Expr.t -> tt option list -> Binsec.Dba.Expr.t -> tt option list -> tt option list

        val byte_bit_merge : tt option -> tt option -> tt option
        val bit_merge : tt option -> tt option -> tt option

        val decide : Instruction.t -> Storage.t list -> t -> action
        val fallback : Instruction.t -> t -> t
    end

module SS = State.Make ()

module Make (TB : TBSig) =
    struct
        include TB

        let propagation ?(propagate_flags = false) ?(propagate_indirect = false) ?(propagate_control = false) instr b =
            let indirect =
                if propagate_indirect
                then indirect
                else fun _ -> None
            in
            let control =
                if propagate_control
                then control
                else fun _ -> None
            in
            let tempstate = ref @@ Utils.StringMap.empty in
            let b = ref b in
            let guide = ref @@ SS.Guide.create instr @@ Dba.of_instruction ~base:instr.Instruction.address instr in
            let update b tempstate g i nexti =
                let expand l =
                    List.fold_right
                        (
                            fun e res ->
                                (List.init 8 (fun _ -> e)) @ res
                        )
                        l []
                in
                let contract l =
                    let _, res = List.fold_right
                        (
                            fun e (i, res) ->
                                if i = 0
                                then ((i + 1) mod 8), e::res
                                else ((i + 1) mod 8), (byte_bit_merge e @@ List.hd res)::(List.tl res)
                        )
                        l (0, [])
                    in
                    res
                in
                let cdiv a b =
                    if (a mod b) > 0
                    then (a / b) + 1
                    else a / b
                in
                let rec get_expr_taint expr =
                    match expr with
                    | Binsec.Dba.Expr.Var(v) ->
                    (
                        let tt =
                            match v.Binsec.Dba.Var.info with
                            | Binsec.Dba.Var.Tag.Temp -> 
                            (
                                try
                                    Utils.StringMap.find v.name tempstate
                                with Not_found -> List.init (v.size) (fun _ -> None)
                            )
                            | Symbol(_)
                            | Flag when not propagate_flags -> List.init (v.size) (fun _ -> None)
                            | _ -> 
                            (
                                match get_taint (Storage.Register(v.name, max 1 @@ cdiv v.size 8)) b with
                                | Some(taint) -> expand taint
                                | _ -> List.init (v.size) (fun _ -> None)
                            )
                        in
                        List.map direct tt
                    )
                    | Load(size, endian, addrexpr, _) ->
                    (
                        let mem_tt =
                            let sto = Storage.Memory(SS.Guide.get_value addrexpr g, size) in
                            match endian, get_taint sto b with
                            | Binsec.Machine.BigEndian, Some(taint) -> List.rev @@ expand taint
                            | _, Some(taint) -> expand taint
                            | _ -> List.init (size * 8) (fun _ -> None)
                        in
                        let addr_tt = indirect @@ List.fold_left bit_merge None @@ get_expr_taint addrexpr in
                        List.map (bit_merge addr_tt) @@ List.map direct mem_tt
                    )
                    | Cst(bv) -> List.map direct @@ List.init (Binsec.Bitvector.size_of bv) (fun _ -> None)
                    | Unary(op, a) -> 
                    (
                        let t = List.map direct @@ get_expr_taint a in
                        unary_taint expr op a t
                    )
                    | Binary(op, a, b) ->
                    (
                        let ta = List.map direct @@ get_expr_taint a in
                        let tb = List.map direct @@ get_expr_taint b in
                        binary_taint expr op a ta b tb
                    )
                    | Ite(cond, a, b) ->
                    (
                        let expr_tt =
                            if SS.Guide.check_assert cond g
                            then get_expr_taint a
                            else get_expr_taint b
                        in
                        let cond_tt = control @@ List.fold_left bit_merge None @@ get_expr_taint cond in
                        List.map (bit_merge cond_tt) @@ List.map direct expr_tt
                    )
                in
                let taint_lv t = function
                    | Binsec.Dba.LValue.Var(v) ->
                    (
                        match v.Binsec.Dba.Var.info with
                        | Binsec.Dba.Var.Tag.Symbol(_)
                        | Flag when not propagate_flags -> b, tempstate
                        | Temp -> b, Utils.StringMap.add v.name t tempstate
                        | _ -> taint_byte (Storage.Register(v.name, max 1 @@ cdiv v.size 8)) (Some(contract t)) b, tempstate
                    )
                    | Restrict(v, itv) ->
                    (
                        let rec splice i res = function
                            | (oldh::oldt), newh::newt ->
                            (
                                if i >= itv.Binsec.Interval.lo && i <= itv.Binsec.Interval.hi
                                then splice (i + 1) (newh::res) (oldt, newt)
                                else splice (i + 1) (oldh::res) (oldt, newh::newt)
                            )
                            | oldh::oldt, [] -> splice (i + 1) (oldh::res) (oldt, [])
                            | [], [] -> List.rev res
                            | _ -> assert false
                        in
                        match v.Binsec.Dba.Var.info with
                        | Binsec.Dba.Var.Tag.Symbol(_)
                        | Flag when not propagate_flags -> b, tempstate
                        | Temp -> 
                        (
                            let t_ = 
                                try
                                    Utils.StringMap.find v.name tempstate
                                with Not_found -> List.init v.size (fun _ -> None)
                            in
                            b, Utils.StringMap.add v.name (splice 0 [] (t_, t)) tempstate
                        )
                        | _ -> 
                        (
                            let t_ =
                                match get_taint (Storage.Register(v.name, max 1 @@ cdiv v.size 8)) b with
                                | Some(taint) -> expand taint
                                | _ -> List.init v.size (fun _ -> None)
                            in
                            taint_byte (Storage.Register(v.name, max 1 @@ cdiv v.size 8)) (Some(contract @@ splice 0 [] (t_, t))) b, tempstate
                        )
                    )
                    | Store(size, endian, addrexpr, _) ->
                    (
                        let addr_tt = indirect @@ List.fold_left bit_merge None @@ get_expr_taint addrexpr in
                        let t = List.map (bit_merge addr_tt) t in
                        let sto = Storage.Memory(SS.Guide.get_value addrexpr g, size) in
                        taint_byte sto (Some(contract t)) b, tempstate
                    )
                in
                match i with
                | Binsec.Dba.Instr.Assign(lv, expr, _) -> taint_lv (get_expr_taint expr) lv
                | Undef(lv, _)
                | Nondet(lv, _) -> taint_lv (List.init (Binsec.Dba.LValue.size_of lv) (fun _ -> None)) lv
                | _ -> b, tempstate
            in
            while not @@ SS.Guide.is_done !guide
            do
                let ng, (nb, ntempstate) = SS.Guide.step (update !b !tempstate !guide) !guide in
                guide := ng;
                b := nb;
                tempstate := ntempstate
            done;
            !b

        let erase instr b =
            List.fold_left (fun b sto -> TB.taint sto None b) b instr.Instruction.writes

        let propagation ?(propagate_flags = false) ?(propagate_indirect = false) ?(propagate_control = false) instr b =
            try
                let inputs = 
                    if propagate_indirect
                    then instr.Instruction.reads
                    else instr.direct_reads
                in
                match decide instr inputs b with
                | Propagate -> propagation ~propagate_flags ~propagate_indirect ~propagate_control instr b
                | Fallback -> fallback instr b
                | Erase -> erase instr b
            with 
            | Dba.DbaFailure(_) -> fallback instr b
            | e ->
            (
                let msg = lazy
                    (
                        let msg = 
                            match e with
                            | Failure(msg) -> msg
                            | _ -> Printexc.to_string e
                        in
                        Printf.sprintf "%s\n%s" (Instruction.pp instr) msg
                    )
                in
                Message.Wrap.send (Message.Wrap.Warning(msg));
                fallback instr b
            )
    end
