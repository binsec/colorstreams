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

module type Sig =
    sig
        module S : State.Sig

        type mode_t = Symbolic | Concrete of int | Concrete_until of Function.CallId.t | Concrete_next | Wait_source

        type tag = Symb

        type byte = Single of int | Range of int * int
        type src_bytes_action = Restrict | Only

        include Analysis.Bank with type Res.value = tag option list

        val set_crwa : bool -> t -> t
        val set_saww : int -> t -> t
        val set_src_bytes : ?action:src_bytes_action -> byte list Utils.StringMap.t -> t -> t
        val get_mode : t -> mode_t
        val get_runtime : t -> float
        val get_state : t -> S.t
        val get_sink_formula : Sink.t -> t -> (StateFormula.Projections.Projection.t * StateFormula.t) option
        val set_concrete : string list -> t -> t
        val add_concrete : string -> t -> t
        val set_state : S.t -> t -> t
        val switch_mode : mode_t -> t -> t
        val next_instruction : Instruction.t -> t -> t
        val next_function : Function.t -> t -> t
        val assign_expr : Storage.t -> Expr.t -> t -> t
    end

module Make (State : State.Sig) =
    struct
        type tag = Symb
        module T =
            struct
                type t = tag

                let create src = 
                    Some(Symb)

                let pp tt =
                    match tt with
                    | Some(Symb) -> "S"
                    | None -> "C"

                let pp_list l =
                    let aux res e =
                        let s = if res = "" then "" else ", " in
                        res ^ s ^ (pp e)
                    in
                    "<" ^
                    (List.fold_left aux "" l) ^
                    ">"

                let equals tt tt_ =
                    match tt, tt_ with
                    | Some(Symb), Some(Symb) -> true
                    | _ -> false

                let compare tt tt_ =
                    if equals tt tt_
                    then Some(0)
                    else None

                let to_generic tt =
                    match tt with
                    | Some(_) -> Result.Bool(true)
                    | None -> Result.Bool(false)
            end

        module ResVal = 
            struct
                type t = T.t option list

                let to_generic r =
                    Result.Lst(List.map T.to_generic r)
            end

        module Res = Result.Make(ResVal)

        module S = State

        module Shadow =
            struct
                module TouchTag =
                    struct
                        type t = Touched

                        let create = function
                            | Storage.Register("rip", _) -> None
                            | _ -> Some(Touched)

                        let from_source src = 
                            match src.Source.storage with
                            | Storage.Register("rip", _) -> []
                            | _ -> [Touched, src.storage]

                        let pp = function
                            | Some(_) -> "T"
                            | _ -> "-"

                        let pp_list _ =
                            ""

                        let compare _ _ =
                            Some(0)

                        let equals _ _ = 
                            true

                        let to_generic _ =
                            Result.Bool(true)
                    end

                module B = Taint.MakeBank (TouchTag)

                type t = B.t

                let create () =
                    B.create ()

                let touch sto b =
                    let tt = TouchTag.create sto in
                    B.taint sto tt b

                let untouch sto b =
                    B.taint sto None b

                let has_touched sto b =
                    match B.get_taint sto b with
                    | Some(_) -> true
                    | None -> false

                let get_untouched sto b =
                    match B.get_taint sto b with
                    | Some(l) ->
                    (
                        let res = ref [] in
                        List.iteri
                            (
                                fun i e -> 
                                    match e with
                                    | Some(_) -> ()
                                    | _ ->
                                    (
                                        match sto with
                                        | Storage.Memory(addr, size) -> 
                                        (
                                            let nsto = Storage.Memory((Address.add_int addr i), 1) in
                                            res := nsto::!res
                                        )
                                        | _ -> ()
                                    )
                            )
                            l
                        ;
                        !res
                    )
                    | None -> [sto]
            end

        type mode_t = Symbolic | Concrete of int | Concrete_until of Function.CallId.t | Concrete_next | Wait_source

        type byte = Single of int | Range of int * int
        type src_bytes_action = Restrict | Only

        type t =
            {
                mode : mode_t;
                state : S.t;
                shadow : Shadow.t;
                concrete : Str.regexp;
                concrete_pattern : string list;
                src_bytes : byte list Utils.StringMap.t;
                src_bytes_action : src_bytes_action;
                crwa : bool;
                saww : int;
                timer : Stats.timer;
                conc_cnt : Stats.counter;
                symb_cnt : Stats.counter
            }

        let create () =
            let mode = Wait_source in
            let state = S.create () in
            let shadow = Shadow.create () in
            let timer = new Stats.timer "se_runtime" "Symbolic execution runtime" in
            let conc_cnt = new Stats.counter "conc_cnt" "Concrete instructions" 0 in
            let symb_cnt = new Stats.counter "symb_cnt" "Symbolic instructions" 0 in
            {mode; state; shadow; concrete = Str.regexp "$"; concrete_pattern = []; src_bytes = Utils.StringMap.empty; src_bytes_action = Restrict; crwa = false; saww = 16; timer; conc_cnt; symb_cnt}

        let add_stats_to_bundle bnk b =
            b
                |> State.add_stats_to_bundle
                |> Stats.Bundle.add_stat (bnk.timer :> Stats.stat)
                |> Stats.Bundle.add_stat (bnk.symb_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (bnk.conc_cnt :> Stats.stat)

        let set_crwa crwa b =
            {b with crwa}

        let set_saww saww b =
            {b with saww}

        let set_src_bytes ?(action = Restrict) src_bytes b =
            {b with src_bytes; src_bytes_action = action}

        let get_mode b =
            b.mode

        let get_runtime b =
            b.timer#get

        let get_state b =
            b.state

        let set_state state b =
            {b with state}

        let set_concrete concrete_pattern b =
            let concrete = Utils.pattern_list concrete_pattern in
            {b with concrete; concrete_pattern}

        let add_concrete func b =
            let concrete_pattern = func::b.concrete_pattern in
            let concrete = Utils.pattern_list concrete_pattern in
            {b with concrete; concrete_pattern}

        let switch_mode mode b =
            let _ =
                match mode with
                | Symbolic -> Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "switching to symbolic mode")))
                | Concrete_until(call_id) -> 
                (
                    let msg = lazy
                        (
                            Printf.sprintf "switching to concrete mode until <%s>" 
                                @@ Function.CallId.pp call_id
                        )
                    in
                    Message.Wrap.send (Message.Wrap.Debug("SE", msg))
                )
                | Concrete(_) -> Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "switching to concrete mode")))
                | Concrete_next -> Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "concretizing next instruction")))
                | Wait_source -> Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "switching to waiting for source mode")))
            in
            {b with mode}

        let touch_and_concr sto b =
            let to_concr = Shadow.get_untouched sto b.shadow in
            List.fold_left
                (
                    fun b sto ->
                        Message.Wrap.send (Message.Wrap.Debug("SE", (lazy ("init: " ^ (Storage.pp sto)))));
                        let state = S.concretize sto b.state in 
                        let shadow = Shadow.touch sto b.shadow in
                        {b with state; shadow}
                )
                b to_concr

        let assign_expr target expr b =
            match b.mode with
            | Symbolic ->
            (
                let stos = Expr.get_stos @@ Expr.simplify expr in
                if List.fold_left (fun res sto -> res || Shadow.has_touched sto b.shadow) false stos
                then
                (
                    let b = List.fold_left 
                        (
                            fun res e -> 
                                match e with
                                | Storage.Custom(_) -> {b with shadow = Shadow.touch e res.shadow}
                                | _ -> touch_and_concr e res
                        ) 
                        b stos 
                    in
                    let symbr, concrr, desync = List.fold_left
                        (
                            fun (symbr, concrr, desync) sto ->
                                try
                                    ignore @@ List.find (fun e -> e) @@ S.is_symbolic sto b.state;
                                    sto::symbr, concrr, desync
                                with
                                | Not_found -> symbr, sto::concrr, desync
                                | S.Desync l -> symbr, concrr, l @ desync
                        )
                        ([], [], []) stos
                    in
                    let b = List.fold_left (fun res e -> let state = S.concretize e res.state in {res with state}) b desync in
                    match symbr with
                    | [] -> b
                    | _ -> {b with state = S.assign_expr target expr b.state; shadow = Shadow.touch target b.shadow}
                )
                else b
            )
            | _ -> b

        (*callee preserved registers in x86_64*)
        let preserved_regs = Str.regexp {|r12\|r13\|r14\|r15|}

        (*find symbolic read addresses and concretize untouched memory values within the restriction window*)
        (*actual restriction of addresses themselves is handled at the symbolic state update level (to avoid impacting global constraints)*)
        (*could do something more efficient but instructions typically have few operands*)
        let check_symb_addresses instr symbr b =
            let mems = List.fold_left
                (
                    fun res sto -> 
                        let addrs = List.filter_map
                            (
                                fun op -> 
                                    match op.Instruction.Operand.kind with
                                    | Component(_, Instruction.Operand.Memory(mem), Instruction.Operand.Read)
                                    | Component(_, Instruction.Operand.Memory(mem), Instruction.Operand.Both) -> 
                                    (
                                        if Instruction.Operand.is_storage op sto
                                        then Some(Storage.Memory(mem.Instruction.Operand.addr, mem.size))
                                        else None
                                    )
                                    | _ -> None
                            )
                            instr.Instruction.operands
                        in
                        List.fold_left
                            (
                                fun res sto ->
                                    try
                                        ignore @@ List.find (fun sto_ -> Storage.compare sto sto_ = 0) res;
                                        res
                                    with Not_found -> sto::res
                            )
                            res addrs
                )
                [] symbr
            in
            List.fold_left
                (
                    fun b sto ->
                        match sto with
                        | Storage.Memory(addr, size) ->
                        (
                            let widened = Storage.Memory(Address.add_int addr (-b.saww), size + 2 * b.saww) in
                            touch_and_concr widened b
                        )
                        | _ -> assert false
                )
                b mems

        let next_instruction instr b =
            b.timer#start;
            let update_concrete ?(preserved = Str.regexp "$") b =
                b.conc_cnt#incr;
                List.fold_left
                    (
                        fun res sto -> 
                            if
                                match sto with
                                | Storage.Register(reg, _) -> Str.string_match preserved reg 0
                                | _ -> false
                            then res
                            else
                                let shadow = Shadow.untouch sto res.shadow in 
                                {res with shadow}
                    ) 
                    b instr.Instruction.writes
            in
            let b =
                match b.mode with
                | Symbolic ->
                (
                    if List.fold_left (fun res sto -> res || Shadow.has_touched sto b.shadow) false instr.Instruction.reads
                    then
                    (
                        let b = List.fold_right touch_and_concr instr.Instruction.reads b in
                        let symbr, concrr, desync = S.get_symbolic_inputs instr b.state in
                        let b = List.fold_left (fun res e -> let state = S.concretize e res.state in {res with state}) b desync in
                        match symbr with
                        | [] -> update_concrete {b with shadow = List.fold_right Shadow.untouch concrr b.shadow}
                        | _ ->
                        (
                            let b = 
                                if b.crwa || b.saww <= 0
                                then b
                                else check_symb_addresses instr symbr b 
                            in
                            try
                                (*prevent issue with partial registers*)
                                let b = List.fold_right touch_and_concr instr.Instruction.writes b in
                                let i = Dba.of_instruction ~base:instr.Instruction.address instr in
                                Message.Wrap.send (Message.Wrap.Debug("SE", (lazy ("dba: \n" ^ (Dba.pp i)))));
                                let state = S.next_instruction ~crwa:b.crwa ~saww:b.saww instr i b.state in
                                b.symb_cnt#incr;
                                {b with state}
                            with e ->
                            (
                                let err =
                                    match e with
                                    | Failure(msg)
                                    | Dba.DbaFailure(msg) -> msg
                                    | e -> Printexc.to_string e
                                in
                                let msg = lazy (Printf.sprintf "could not update symbolic state:\n%s\nsymbolic bytes lost (from <%s>)" err @@ Utils.pp_list Storage.pp symbr) in
                                Message.Wrap.send (Message.Wrap.BigWarning(msg));
                                Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "concretizing writes")));
                                update_concrete b
                            )
                        )
                    )
                    else update_concrete b
                )
                | Concrete_until(_)
                | Concrete(_) -> update_concrete b
                | Concrete_next -> switch_mode Symbolic @@ update_concrete b
                | Wait_source -> 
                (
                    b.conc_cnt#incr;
                    b
                )
            in
            b.timer#stop;
            b

        let next_function func b =
            b.timer#start;
            let b =
                match func, b.mode with
                | Function.Entry({fname = "Concrete_begin"; _}), Symbolic -> switch_mode (Concrete(1)) b
                | Function.Entry({fname = "Concrete_begin"; _}), Concrete(n) -> {b with mode = Concrete(n + 1)}
                | Function.Entry({fname = "Concrete_end"; _}), Concrete(n) ->
                (
                    if n > 1
                    then {b with mode = Concrete(n - 1)}
                    else switch_mode Symbolic b
                )
                | Function.Entry({fname; call_id = Some(call_id); _}), Symbolic ->
                (
                    if Str.string_match b.concrete fname 0
                    then switch_mode (Concrete_until(call_id)) b
                    else b
                )
                | Function.CallRet(call_id, _), Concrete_until(call_id2) ->
                (
                    if call_id = call_id2 || call_id.Function.CallId.stack_id > call_id2.stack_id
                    then switch_mode Symbolic b
                    else b
                )
                | _ -> b
            in
            b.timer#stop;
            b

        let source src b =
            let mode = if b.mode = Wait_source then Symbolic else b.mode in
            let tag = Identifier.pp_basic src.Source.name in
            let stos = 
                try
                    List.map
                        (
                            fun byte ->
                                match src.Source.storage, byte with
                                | Storage.Memory(addr, size), Single(n) ->
                                (
                                    if n >= size then raise (Failure (Printf.sprintf "cannot select byte %d from source %s: out of range" n @@ Source.pp src));
                                    Message.Wrap.send (Message.Wrap.SmallInfo (lazy (Printf.sprintf "selecting byte %d from source %s" n @@ Source.pp src)));
                                    Storage.Memory(Address.add_int addr n, 1)
                                )
                                | Storage.Memory(addr, size), Range(lo, hi) ->
                                (
                                    if lo > hi || lo < 0 || hi >= size
                                    then raise (Failure (Printf.sprintf "cannot select bytes %d to %d from source %s: invalid range" lo hi @@ Source.pp src))
                                    ;
                                    Message.Wrap.send (Message.Wrap.SmallInfo (lazy (Printf.sprintf "selecting bytes %d to %d from source %s" lo hi @@ Source.pp src)));
                                    Storage.Memory(Address.add_int addr lo, hi - lo + 1)
                                )
                                | _ -> src.storage
                        )
                        @@ Utils.StringMap.find (Identifier.pp src.Source.name) b.src_bytes
                with Not_found ->
                (
                    match b.src_bytes_action with
                    | Restrict -> [src.Source.storage]
                    | Only -> []
                )
            in
            let state, shadow = List.fold_left 
                (
                    fun (state, shadow) sto ->
                        (*let state = S.symbolize ~tag sto state in*)
                        let _, state = S.make_input tag src.Source.desc sto state in
                        let shadow = Shadow.touch sto shadow in
                        state, shadow
                )
                (b.state, b.shadow) stos
            in
            {b with mode; state; shadow}

        let get_sink_formula snk b =
            let name = Identifier.pp_basic snk.Sink.name in
            let b = List.fold_right touch_and_concr (Expr.get_stos snk.Sink.expr) b in
            try
                let b = 
                    match snk.constr with
                    | Some(sto) -> 
                    (
                        let b = touch_and_concr sto b in
                        let state = S.assume sto b.state in 
                        {b with state}
                    )
                    | _ -> b
                in
                let proj, s = S.make_projection ~name snk.expr b.state in
                let proj_filter = Some(fun e -> StateFormula.Projections.Projection.equals e proj) in
                Some(proj, S.get_formula ~proj_filter s)
            with err -> 
            (
                Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printf.sprintf "Could not get sink formula (%s)" @@ Printexc.to_string err)));
                None
            )

        let sink snk b =
            let stats = Stats.Bundle.create ""
                |> Stats.Bundle.add_stat ((Oo.copy b.timer) :> Stats.stat)
                |> Stats.Bundle.add_stat ((Trace.get_runtime ()) :> Stats.stat)
            in
            let stats = Some(stats) in
            match b.mode with
            | Symbolic ->
            (
                let b = List.fold_right touch_and_concr (Expr.get_stos snk.Sink.expr) b in
                try
                    let some = ref false in
                    let symb = S.is_expr_symbolic snk.Sink.expr b.state in
                    let check_byte b =
                        if b
                        then 
                        (
                            some := true;
                            Some(Symb)
                        )
                        else None
                    in
                    let res = List.map check_byte symb in
                    Res.create ~label:"Symbolic" ~stats res, !some
                with e -> 
                (
                    let msg =
                        match e with
                        | S.Desync(_) -> "symbolic state desync"
                        | Failure(msg) -> msg
                        | _ -> Printexc.to_string e
                    in
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy msg));
                    Res.create ~label:"Symbolic" ~stats [None], false
                )
            )
            | _ -> Res.create ~label:"Symbolic" ~stats [None], false

        let pp b =
            S.pp_formula b.state
    end
