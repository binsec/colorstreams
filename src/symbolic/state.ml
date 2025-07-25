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
        type t

        exception Desync of Storage.t list

        module Guide :
            sig
                type guide

                val create : Instruction.t -> Dba.t -> guide
                val check_assert : Binsec.Dba.Expr.t -> guide -> bool
                val get_value : Binsec.Dba.Expr.t -> guide -> Z.t
                val step : (Binsec.Dba.Instr.t -> int -> 'a) -> guide -> guide * 'a
                val is_done : guide -> bool
            end

        val create : unit -> t
        val symbolize : ?tag:string -> Storage.t -> t -> t
        val concretize : Storage.t -> t -> t
        val assume : Storage.t -> t -> t
        val is_symbolic : Storage.t -> t -> bool list
        val assign_expr : Storage.t -> Expr.t -> t -> t
        val is_expr_symbolic : Expr.t -> t -> bool list
        val get_symbolic_inputs : Instruction.t -> t -> Storage.t list * Storage.t list * Storage.t list
        val next_instruction : ?crwa:bool -> ?saww:int -> Instruction.t -> Dba.t -> t -> t
        val make_input : string -> string list -> Storage.t -> t -> StateFormula.Inputs.Input.t * t
        val get_inputs : t -> bool StateFormula.Inputs.t
        val make_projection : ?name:string -> ?suffix:string -> Expr.t -> t -> StateFormula.Projections.Projection.t * t
        val get_projections : t -> bool StateFormula.Projections.t
        val byte_restrict : ?lo:(int option) -> ?hi:(int option) -> Storage.t -> t -> t
        val pp_formula : t -> string
        val get_formula : ?proj_filter:((StateFormula.Projections.Projection.t -> bool) option) -> t -> StateFormula.t

        val add_stats_to_bundle : Stats.Bundle.t -> Stats.Bundle.t
    end

module Query_stats () = 
    struct
        module Preprocess = 
            struct
                let sat_cnt = new Stats.counter "sat cnt" "Preprocessed sat" 0
                let unsat_cnt = new Stats.counter "unsat cnt" "Preprocessed unsat" 0
                let const_cnt = new Stats.counter "const cnt" "Preprocessed const" 0

                let get_true () = 
                    sat_cnt#get

                let get_false () =
                    unsat_cnt#get

                let get_const () =
                    const_cnt#get

                let total () =
                    sat_cnt#get + unsat_cnt#get + const_cnt#get

                let incr_true () =
                    sat_cnt#incr

                let incr_false () =
                    unsat_cnt#incr

                let incr_const () =
                    const_cnt#incr

                let reset () =
                    sat_cnt#set 0;
                    unsat_cnt#set 0;
                    const_cnt#set 0

                let add_to_bundle b =
                    b
                        |> Stats.Bundle.add_stat (sat_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (unsat_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (const_cnt :> Stats.stat)

                let pp _ () =
                    ()

                let to_toml () =
                    raise (Failure "Not implemented")
            end

        module Solver = 
            struct
                let sat_cnt = new Stats.counter "sat cnt" "Solver sat" 0
                let unsat_cnt = new Stats.counter "unsat cnt" "Solver unsat" 0
                let err_cnt = new Stats.counter "err cnt" "Solver error" 0
                let timer = new Stats.timer "solver runtime" "Solver runtime"

                let get_sat () = 
                    sat_cnt#get

                let get_unsat () =
                    unsat_cnt#get

                let get_err () =
                    err_cnt#get

                let get_time () =
                    timer#get

                let incr_sat () =
                    sat_cnt#incr

                let incr_unsat () =
                    unsat_cnt#incr

                let incr_err () =
                    err_cnt#incr

                let start_timer () =
                    timer#start

                let stop_timer () =
                    timer#stop

                let add_to_bundle b =
                    b
                        |> Stats.Bundle.add_stat (sat_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (unsat_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (err_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (timer :> Stats.stat)

                let pp _ () =
                    ()

                let to_toml () =
                    raise (Failure "Not implemented")
            end

        let add_to_bundle b =
            b
                |> Preprocess.add_to_bundle 
                |> Solver.add_to_bundle

        let reset () =
            ()

        let pp _ () =
            ()
    end

module Make () =
    struct
        module Flags =
            struct
                let flags = 
                    [
                        ("cf", "0x1");
                        ("pf", "0x4");
                        ("af", "0x10");
                        ("zf", "0x40");
                        ("sf", "0x80");
                        ("tf", "0x100");
                        ("if", "0x200");
                        ("df", "0x400");
                        ("of", "0x800")
                    ]

                let split rflags =
                    let aux res (flag, mask) =
                        let value = 
                            if Z.(rflags land (Z.of_string mask)) > Z.zero 
                            then Z.one
                            else Z.zero
                        in
                        (flag, value)::res
                    in
                    List.fold_left aux [] flags
            end

        module QS = Query_stats ()
        module Path = Libsse.Path.Make ()
        module State = 
            struct
                include Libterm.Senv.State (Domains.Interval) (Libterm.Solver.Once (val Libterm.Senv.get_solver ~solver:Smt.Smt_options.Bitwuzla_legacy ())) (QS)

                let id = Path.register_key Uid.zero

                let symbols = Path.register_key Libsse.Types.S.Map.empty
            end
        module Eval = Libsse.Eval.Make (Path) (State)

        type t = 
            {
                state : State.t;
                path : Path.t;
                projs : bool StateFormula.Projections.t;
                inputs : bool StateFormula.Inputs.t
            }

        let create () = 
            let state = State.empty () in
            let path = Path.empty () in
            let projs = StateFormula.Projections.empty in
            let inputs = StateFormula.Inputs.empty in
            {state; path; projs; inputs}

        let pp_formula s =
            State.pp_smt None Format.str_formatter s.state;
            Format.flush_str_formatter ()

        let get_formula ?(proj_filter = None) s =
            let projections =
                match proj_filter with
                | Some(filter) -> StateFormula.Projections.update (StateFormula.Projections.ProjMap.filter (fun proj _ -> filter proj)) s.projs
                | _ -> s.projs
            in
            StateFormula.create ~inputs:s.inputs ~projections @@ State.to_formula s.state

        let assignment ~lvalue ~rvalue s =
            let state =
                match lvalue with
                | Binsec.Dba.LValue.Var(var) -> Eval.assign var rvalue s.state s.path
                | Binsec.Dba.LValue.Restrict(var, {lo; hi}) -> Eval.assign var
                    (Binsec.Dba_utils.Expr.complement rvalue ~hi ~lo var)
                    s.state s.path
                | Binsec.Dba.LValue.Store(_, dir, addr, _) -> Eval.write ~addr rvalue dir s.state s.path
            in
            {s with state}

        let nondet ?(tag = "unknown") ?(tag_name = false) ~lvalue s =
            let state = 
                let get_tag name = if tag_name then name else tag in
                match lvalue with
                | Binsec.Dba.LValue.Var(var) -> Eval.fresh var s.state s.path
                | Binsec.Dba.LValue.Restrict(var, {lo; hi}) ->
                (
                    let name = get_tag var.name in
                    let tag = Binsec.Dba.Var.Tag.Empty in
                    let bitsize = Binsec.Size.Bit.create (hi - lo + 1) in
                    let nondet = Binsec.Dba.Var.create name ~bitsize ~tag in
                    Eval.assign var
                        (Binsec.Dba_utils.Expr.complement (Binsec.Dba.Expr.v nondet) ~hi ~lo var)
                        (Eval.fresh nondet s.state s.path)
                        s.path
                )
                | Binsec.Dba.LValue.Store (bytesize, dir, addr, _) ->
                (
                    let name = tag in
                    let tag = Binsec.Dba.Var.Tag.Empty in
                    let bitsize = Binsec.Size.Bit.create (8 * bytesize) in
                    let nondet = Binsec.Dba.Var.create name ~bitsize ~tag in
                    Eval.write ~addr (Binsec.Dba.Expr.v nondet) dir (Eval.fresh nondet s.state s.path) s.path
                )
            in
            {s with state}

        let assumption cond s =
            let msg = lazy
                (
                    "assume: " ^
                    (
                        Binsec.Dba_printer.Ascii.pp_expr Format.str_formatter cond;
                        Format.flush_str_formatter ()
                    )
                )
            in
            Message.Wrap.send (Message.Wrap.Debug("SE", msg));
            try
                match Eval.assume cond s.state s.path with
                | Some(state) -> 
                (
                    Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assume: sat")));
                    {s with state}
                )
                | None -> 
                (
                    Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assume: unsat")));
                    raise (Failure "assume: unsat")
                )
            with Libsse.Types.Unknown -> 
            (
                Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assume: unknown")));
                raise (Failure "assume: unknown")
            )

        let lval_of_sto = function
            | Storage.Custom(name, size, _)
            | Register(name, size) -> 
            (
                let bitsize = Binsec.Size.Bit.of_int32 @@ Int32.of_int (size * 8) in
                Binsec.Dba.LValue.var ~bitsize name
            )
            | Memory(addr, size) ->
            (
                let dir = Binsec.Kernel_options.Machine.endianness () in
                let bv_addr = Binsec.Dba.Expr.constant (Address.to_bv addr) in
                let size = Binsec.Size.Byte.of_int32 (Int32.of_int size) in
                Binsec.Dba.LValue.store size dir bv_addr
            )

        let expr_of_sto = function
            | Storage.Custom(name, size, _)
            | Register(name, size) -> Binsec.Dba.Expr.var name (size * 8)
            | Memory(addr, size) -> Binsec.Dba.Expr.load (Binsec.Size.Byte.create size) (Binsec.Kernel_options.Machine.endianness ()) @@ Binsec.Dba.Expr.constant @@ Address.to_bv addr

        let assume sto s =
            let cond = Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.GtU (expr_of_sto sto)
                @@ Binsec.Dba.Expr.zeros ((Storage.size sto) * 8)
            in
            assumption cond s

        let assertion cond s =
            let msg = lazy
                (
                    "assert: " ^
                    (
                        Binsec.Dba_printer.Ascii.pp_expr Format.str_formatter cond;
                        Format.flush_str_formatter ()
                    )
                )
            in
            Message.Wrap.send (Message.Wrap.Debug("SE", msg));
            try
                let res = Eval.test cond s.state s.path in
                match res with
                | True _ -> 
                (
                    Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assert: true")));
                    res
                )
                | False _ -> 
                (
                    Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assert: unsat")));
                    res
                )
                | Both _ ->
                (
                    Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assert: sat")));
                    res
                )
            with Libsse.Types.Unknown -> 
            (
                Message.Wrap.send (Message.Wrap.Debug("SE", (lazy "assert: unknown")));
                raise (Failure "assert: unknown")
            )

        let dba_of_expr expr =
            let rec mk_expr = function
                | Expr.Const(z, size) -> Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create z (size * 8)
                | Var(sto) -> expr_of_sto sto
                | Unop(op, a) ->
                (
                    match op with
                    (*does not work*)
                    | Expr.Restrict(b, e) -> Binsec.Dba.Expr.restrict (b * 8) (e * 8 + 7) @@ mk_expr a
                    | Expr.Extend(s) -> Binsec.Dba.Expr.uext (s * 8) @@ mk_expr a
                    | Expr.Strlen(s) -> mk_expr (Expr.Const(s, 8))
                )
                | Bnop(op, a, b) ->
                (
                    match op with
                    | Expr.Add -> Binsec.Dba.Expr.add (mk_expr a) @@ mk_expr b
                    | Sub -> Binsec.Dba.Expr.sub (mk_expr a) @@ mk_expr b
                    | Mul -> Binsec.Dba.Expr.mul (mk_expr a) @@ mk_expr b
                    | Max -> 
                    (
                        let a = mk_expr a in
                        let b = mk_expr b in
                        Binsec.Dba.Expr.ite (Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.GtU a b) a b
                    )
                    | Min -> 
                    (
                        let a = mk_expr a in
                        let b = mk_expr b in
                        Binsec.Dba.Expr.ite (Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.GtU a b) b a
                    )
                    | Dist ->
                    (
                        let a = mk_expr a in
                        let b = mk_expr b in
                        Binsec.Dba.Expr.ite (Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.GtU a b) (Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.Minus a b) @@ Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.Minus b a
                    )
                )
            in
            mk_expr @@ Expr.simplify expr

        let assign_expr sto expr s =
            assert (Storage.size sto = Expr.size expr);
            let rvalue = dba_of_expr expr in
            let lvalue = lval_of_sto sto in
            assignment ~lvalue ~rvalue s

        let symbolize ?(tag = "unknown") sto s =
            let lvalue = lval_of_sto sto in
            nondet ~tag ~lvalue s

        let concretize sto s =
            let do_concr sto value s =
                let lvalue = lval_of_sto sto in
                let rvalue = Binsec.Dba.Expr.constant 
                    @@ Binsec.Bitvector.create value ((Storage.size sto) * 8)
                in
                assignment ~lvalue ~rvalue s
            in
            let value = Trace.stoval sto in
            match sto with
            | Storage.Register(reg, size) -> 
            (
                match reg with
                | "rflags"
                | "eflags"
                | "flags" -> List.fold_left 
                    (
                        fun s (flag, value) -> 
                            let bitsize = Binsec.Size.Bit.of_int32 @@ Int32.of_int 1 in
                            let lvalue = Binsec.Dba.LValue.var ~bitsize flag in
                            let rvalue = Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create value 1 in
                            assignment ~lvalue ~rvalue s
                    )
                    s
                        @@ Flags.split value
                | "cs"
                | "ds"
                | "ss"
                | "es" -> s 
                    |> do_concr sto value
                    |> do_concr (Storage.Register(reg ^ "_base", 8)) (Z.shift_left value 32)
                | "fs"
                | "gs" ->
                (
                    let baseval = Trace.stoval (Storage.Register("seg_" ^ reg ^ "_base", 8)) in
                    s
                        |> do_concr sto value
                        |> do_concr (Storage.Register(reg ^ "_base", 8)) baseval
                )
                | _ -> do_concr sto value s
            )
            | _ -> do_concr sto value s

        exception Desync of Storage.t list

        let check_symbolic expr concrete s =
            let cond = Binsec.Dba.Expr.binary Binsec.Dba.Binary_op.Eq expr concrete in
            try
                match assertion cond s with
                | True _ -> false
                | Both _ -> true
                | False _ -> raise (Failure "unsat")
            with e ->
            (
                let msg = 
                    match e with
                    | Failure msg -> msg
                    | e -> Printexc.to_string e
                in
                Binsec.Dba_printer.Ascii.pp_expr Format.str_formatter expr;
                raise (Failure (Printf.sprintf "error when checking <%s> (%s)" (Format.flush_str_formatter ()) msg))
            )

        let is_mem_symbolic addr size concrete s =
            let dir = Binsec.Kernel_options.Machine.endianness () in
            let bv_addr = Binsec.Dba.Expr.constant (Address.to_bv addr) in
            let size_ = Binsec.Size.Byte.of_int32 (Int32.of_int size) in
            let load = Binsec.Dba.Expr.load size_ dir bv_addr in
            let cst = Binsec.Dba.Expr.constant (Binsec.Bitvector.create concrete (8 * size)) in
            check_symbolic load cst s

        let is_var_symbolic name size concrete s =
            let aux bs res (name, value) =
                let var = Binsec.Dba.Expr.var name bs in
                let cst = Binsec.Dba.Expr.constant (Binsec.Bitvector.create value bs) in
                (check_symbolic var cst s) || res
            in
            match name with
            | "flags"
            | "eflags"
            | "rflags" -> List.fold_left (aux 1) false @@ Flags.split concrete
            | _ -> aux (size * 8) false (name, concrete)

        let is_symbolic sto s =
            let value = Trace.stoval sto in
            match sto with
            | Storage.Custom(name, size, _)
            | Storage.Register(name, size) -> 
            (
                try
                    let res = is_var_symbolic name size value s in
                    List.init size (fun _ -> res)
                with Failure msg ->
                (
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy msg));
                    raise (Desync [sto])
                )
            )
            | Storage.Memory(addr, size) ->
            (
                let vlist = Utils.bytes_of_z value size in
                let rec aux a l =
                    match l with
                    | h::t -> 
                    (
                        try
                            let res = is_mem_symbolic a 1 h s in
                            let l, err = aux (Address.incr a) t in
                            res::l, err
                        with Failure msg ->
                        (
                            Message.Wrap.send (Message.Wrap.BigWarning(lazy msg));
                            let l, err = aux (Address.incr a) t in
                            l, sto::err
                        )
                    )
                    | _ -> [], []
                in
                let res, err = aux addr vlist in
                match err with
                | [] -> res
                | _ -> raise (Desync err)
            )

        let is_expr_symbolic expr s =
            match expr with
            | Expr.Const(_, size) -> List.init size (fun _ -> false)
            | Var(sto) -> is_symbolic sto s
            | _ -> 
            (
                let sto = Storage.Custom("expr_check", Expr.size expr, Trace.exprval expr) in
                let s = assign_expr sto expr s in
                is_symbolic sto s
            )

        let get_symbolic_inputs i s =
            let aux (symb, concr, desync) sto =
                try
                    ignore @@ List.find (fun e -> e) @@ is_symbolic sto s;
                    sto::symb, concr, desync
                with
                | Not_found -> symb, sto::concr, desync
                | Desync l -> symb, concr, l @ desync
            in
            List.fold_left aux ([], [], []) i.Instruction.reads

        module Guide =
            struct
                type guide = 
                    {
                        state : t;
                        dba : Binsec.Dhunk.t;
                        idx : int
                    }

                let create instr dba =
                    Message.Wrap.send (Message.Wrap.Debug("SE", (lazy ("creating guide for:\n " ^ (Dba.pp dba)))));
                    let state = List.fold_right concretize instr.Instruction.reads @@ create () in
                    {state; dba = Binsec.Instruction.hunk dba; idx = Binsec.Instruction.start dba}

                let check_assert cond g =
                    match assertion cond g.state with
                    | True _ -> true
                    | False _ -> false
                    | Both _ -> raise (Failure "guide: symbolic condition")

                let get_value expr g =
                    match Eval.split_on expr ~n:1 ~except:[] g.state.state g.state.path with
                    | [bv, _] -> Binsec.Bitvector.value_of bv
                    | _ -> assert false

                let step_guide g =
                    let idx, state =
                        match Binsec.Dhunk.inst g.dba g.idx with
                        | Some(Binsec.Dba.Instr.Assign(lvalue, rvalue, idx)) -> idx, assignment ~lvalue ~rvalue g.state
                        | Some(Binsec.Dba.Instr.Undef(lvalue, idx))
                        | Some(Binsec.Dba.Instr.Nondet(lvalue, idx)) -> idx, nondet ~tag_name:true ~lvalue g.state
                        | Some(Binsec.Dba.Instr.If(cond, jump, next)) -> 
                        (
                            let ncond = Binsec.Dba.Expr.unary Binsec.Dba.Unary_op.Not cond in
                            match assertion cond g.state with
                            | True _ ->
                            (
                                match jump with
                                | JInner idx -> idx, assumption cond g.state
                                | JOuter _ -> (-1), assumption cond g.state
                            )
                            | False ss -> next, assumption ncond g.state
                            | Both ss -> raise (Failure "guide: symbolic split")
                        )
                        | Some(Binsec.Dba.Instr.SJump(JInner idx, _)) -> idx, g.state
                        | Some(Binsec.Dba.Instr.SJump(JOuter _, _))
                        | Some(Binsec.Dba.Instr.DJump(_, _)) -> (-1), g.state
                        | Some(Binsec.Dba.Instr.Stop(_)) -> raise (Failure "guide: stop")
                        | Some(_) -> raise (Failure "guide: not implemented")
                        | _ -> (-1), g.state
                    in
                    {g with idx; state}

                let step f g =
                    try
                        Message.Wrap.send (Message.Wrap.Debug("SE", lazy "updating guide"));
                        let ng = step_guide g in
                        Message.Wrap.send (Message.Wrap.Debug("SE", lazy "following guide"));
                        match Binsec.Dhunk.inst g.dba g.idx with
                        | Some(i) -> ng, f i ng.idx
                        | _ -> assert false
                    with e ->
                    (
                        let err = 
                            match e with
                            | Failure msg -> msg
                            | e -> Printexc.to_string e
                        in
                        Binsec.Dhunk.pp Format.str_formatter g.dba;
                        let msg = Printf.sprintf "%s> line %d: %s"
                            (Format.flush_str_formatter ())
                            g.idx err
                        in
                        raise (Failure msg)
                    )

                let is_done g =
                    g.idx = (-1)
            end

        let next_instruction ?(crwa = false) ?(saww = 16) instr dba s =
            let guide = ref @@ Guide.create instr dba in
            let s = ref s in
            let update oldg s i nexti =
                let i =
                    (*local restriction of symbolic memory addresses (crwa = concretize rw addresses, saww = symbolic address window width i.e. restriction window)*)
                    (*this means that constraints on address components are unaffected*)
                    (*this may introduce some inconsistencies if those data-flows are recombined with those from the affected loads and writes later, but unlikely? and preserves path width*)
                    (*add a variant affecting global constraints + option?*)
                    let mk_cst e addr cst =
                        let msg = lazy
                            (
                                let instr = 
                                    Binsec.Dba_printer.Ascii.pp_instruction Format.str_formatter i;
                                    Format.flush_str_formatter ()
                                in
                                let expr =
                                    Binsec.Dba_printer.Ascii.pp_expr Format.str_formatter e;
                                    Format.flush_str_formatter ()
                                in
                                Printf.sprintf "replacing address <%s> in <%s> with concrete value <%s>" expr instr @@ Address.to_string_hex addr
                            )
                        in
                        Message.Wrap.send (Message.Wrap.Debug("SE", msg));
                        cst
                    in
                    let mk_window e addr =
                        let msg = lazy
                            (
                                let instr = 
                                    Binsec.Dba_printer.Ascii.pp_instruction Format.str_formatter i;
                                    Format.flush_str_formatter ()
                                in
                                let expr =
                                    Binsec.Dba_printer.Ascii.pp_expr Format.str_formatter e;
                                    Format.flush_str_formatter ()
                                in
                                Printf.sprintf "constraining address <%s> in <%s> to a %d bytes wide window" expr instr (2 * saww + 1)
                            )
                        in
                        Message.Wrap.send (Message.Wrap.Debug("SE", msg));
                        List.init
                            (2 * saww + 1)
                            (
                                fun i ->
                                    Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create (Address.add_int (Address.add_int addr (-saww)) i) 64
                            )
                        (*let lo = Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create (Address.add_int addr (-saww)) 64 in
                        let hi = Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create (Address.add_int addr saww) 64 in
                        let addr = Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create addr 64 in
                        let cond1 = Binsec.Dba.Expr.binary (Binsec.Dba.Binary_op.LtU) e lo in
                        let cond2 = Binsec.Dba.Expr.binary (Binsec.Dba.Binary_op.GtU) e hi in
                        Binsec.Dba.Expr.ite cond1 addr @@ Binsec.Dba.Expr.ite cond2 addr e*)
                    in
                    let check_addr rw cons e =
                        let addr = Guide.get_value e oldg in
                        let cst = Binsec.Dba.Expr.constant @@ Binsec.Bitvector.create addr 64 in
                        if check_symbolic e cst s
                        then
                        (
                            let msg = lazy (rw ^ " memory access at symbolic address") in
                            Message.Wrap.send (Analysis.Wrap.WithLoc(Message.Wrap.Warning(msg)));
                            if crwa || saww == 0
                            then cons @@ mk_cst e addr cst
                            else if saww > 0 
                            then
                            ( 
                                List.fold_left
                                    (
                                        fun res cst ->
                                            Binsec.Dba.Expr.ite (Binsec.Dba.Expr.binary (Binsec.Dba.Binary_op.Eq) e cst) (cons cst) res
                                    )
                                    (cons cst)
                                    @@ mk_window e addr
                            )
                            else 
                            (
                                Message.Wrap.send (Message.Wrap.Warning(lazy "symbolic memory address left unrestricted, consider setting a restriction window width (saww) or selecting systematic concretization (crwa)"));
                                cons e
                            )
                        )
                        (*may seem useless but sometimes helps optimize*)
                        else cons cst
                    in
                    let visitor =
                        object(self)
                            inherit Dba.Visitor.visitor

                            method visit_expr_after = function
                                | Binsec.Dba.Expr.Load(s, endian, addr, array) -> 
                                (
                                    let size = Binsec.Size.Byte.of_int32 @@ Int32.of_int s in
                                    (*restriction window note: binsec cannot optimize the array away if the ite chain is inside the load address expression (very important for performance)*)
                                    check_addr "read" (fun e -> Binsec.Dba.Expr.load ?array size endian e) addr
                                )
                                | e -> e

                            method visit_lv_after = function
                                | Binsec.Dba.LValue.Store(s, endian, addr, array) -> 
                                (
                                    let size = Binsec.Size.Byte.of_int32 @@ Int32.of_int s in
                                    (*restriction window note: cannot put the ite chain outside of the store unfortunately, this will result in sub par performance with memory writes at symbolic addresses (always bad anyways?)*)
                                    Binsec.Dba.LValue.store ?array size endian @@ check_addr "write" (fun e -> e) addr
                                )
                                | e -> e
                        end
                    in
                    Dba.Visitor.visit_instr visitor i
                in
                match i with
                | Binsec.Dba.Instr.Assign(lvalue, rvalue, _) -> assignment ~lvalue ~rvalue s
                | Binsec.Dba.Instr.Undef (lvalue, _)
                | Binsec.Dba.Instr.Nondet(lvalue, _) -> nondet ~tag_name:true ~lvalue s
                | Binsec.Dba.Instr.If (cond, jump, next) -> 
                (
                    let ncond = Binsec.Dba.Expr.unary Binsec.Dba.Unary_op.Not cond in
                    if nexti = next
                    then assumption ncond s
                    else
                    (
                        let jidx =
                            match jump with
                            | JInner idx -> idx
                            | JOuter _ -> (-1)
                        in
                        if nexti = jidx
                        then assumption cond s
                        else raise (Failure (Printf.sprintf "jump targets <%d, %d> do not match guide index <%d>" jidx next nexti))
                    )
                )
                | Binsec.Dba.Instr.SJump (JInner _, _)
                | Binsec.Dba.Instr.SJump (JOuter _, _)
                | Binsec.Dba.Instr.DJump (_, _) -> s
                | Binsec.Dba.Instr.Stop(_) -> raise (Failure "stop")
                | _ -> raise (Failure "not implemented")
            in
            while not @@ Guide.is_done !guide
            do
                let ng, ns = Guide.step (update !guide !s) !guide in
                guide := ng;
                s := ns
            done;
            !s

        let make_input name desc sto s =
            let input, inputs = StateFormula.Inputs.add name desc true s.inputs in
            let s = symbolize ~tag:(StateFormula.Inputs.Input.pp input) sto s in
            input, {s with inputs}

        let get_inputs s =
            s.inputs

        let make_projection ?(name = "") ?(suffix = "proj") expr s =
            let name =
                if name = ""
                then
                (
                    match expr with
                    | Expr.Var(Storage.Custom(name, _, _))
                    | Expr.Var(Storage.Register(name, _)) -> "__" ^ name
                    | Expr.Var(Storage.Memory(addr, _)) -> "__" ^ (Address.to_string_hex addr)
                    | _ -> "expr"
                )
                else name
            in
            let proj, projs = StateFormula.Projections.add ~suff:suffix name true s.projs in
            let projsto = Storage.Custom(StateFormula.Projections.Projection.pp proj, Expr.size expr, Trace.exprval expr) in
            let s = assign_expr projsto expr s in
            proj, {s with projs}

        let get_projections s =
            s.projs

        let byte_restrict ?(lo = None) ?(hi = None) sto s =
            let aux op bnd s =
                match bnd with
                | Some(bnd) ->
                (
                    let var, size =
                        match sto with
                        | Storage.Custom(name, size, _) 
                        | Storage.Register(name, size) -> Binsec.Dba.Expr.var name (8 * size), size
                        | Storage.Memory(addr, size) ->
                        (
                            let dir = Binsec.Kernel_options.Machine.endianness () in
                            let size_ = Binsec.Size.Byte.of_int32 (Int32.of_int size) in
                            let bv_addr = Binsec.Dba.Expr.constant (Address.to_bv addr) in
                            Binsec.Dba.Expr.load size_ dir bv_addr, size
                        )
                    in
                    let bnd = 
                        if bnd = 0
                        then Z.zero
                        else
                        (
                            let zbnd = Z.shift_left Z.one (8 * bnd) in
                            if op = Binsec.Dba.Binary_op.GeqU
                            then zbnd
                            else Z.sub zbnd Z.one
                        )
                    in
                    let bv_bnd = Binsec.Bitvector.create bnd (size * 8) in
                    let vbnd = Binsec.Dba.Expr.constant bv_bnd in
                    let cond = Binsec.Dba.Expr.binary op var vbnd in
                    assumption cond s
                )
                | None -> s
            in
            s
                |> aux Binsec.Dba.Binary_op.GeqU lo
                |> aux Binsec.Dba.Binary_op.LeqU hi

        let add_stats_to_bundle = QS.add_to_bundle
    end
