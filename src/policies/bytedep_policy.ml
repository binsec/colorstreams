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

let indirect_opt = new Options.TaggedOptions.opt false
let control_opt = new Options.TaggedOptions.opt false
let flag_opt = new Options.TaggedOptions.opt false
let drop_opt = new Options.TaggedOptions.opt ""

module TT =
    struct
        type kind = Direct | Indirect | Control

        type t = 
            {
                src : Source.t;
                kind : kind;
                byte_collection : Collection.t;
            }

        let from_source src =
            match src.Source.storage with
            | Storage.Memory(addr, size) -> List.init size (fun byte -> {src; kind = Direct; byte_collection = Collection.single @@ Z.of_int byte}, Storage.Memory((Address.add_int addr byte), 1))
            | _ -> [{src; kind = Direct; byte_collection = Collection.interval @@ Interval.create ~lo:Z.zero ~hi:(Z.of_int @@ Storage.size src.storage)}, src.storage]

        let pp_kind = function
            | Direct -> "direct"
            | Indirect -> "indirect"
            | Control -> "control"

        let pp_kind_short = function
            | Direct -> "D"
            | Indirect -> "I"
            | Control -> "C"

        let pp = function
            | Some(tt) -> Printf.sprintf "%s(%s)[%s]" (pp_kind_short tt.kind) (Source.pp tt.src) @@ Collection.pp tt.byte_collection
            | None -> "-"

        let same_kind a b =
            match a.kind, b.kind with
            | Direct, Direct
            | Indirect, Indirect
            | Control, Control -> true
            | _ -> false

        let equals a b =
            match a, b with
            | (Some(a)), Some(b) -> Identifier.equals a.src.Source.name b.src.Source.name && Collection.equals a.byte_collection b.byte_collection && same_kind a b
            | _ -> false

        let merge a b =
            if Identifier.equals a.src.Source.name b.src.Source.name && same_kind a b
            then Some({a with byte_collection = Collection.merge a.byte_collection b.byte_collection})
            else None

        let relegate_kind target tt =
            let kind =
                match tt.kind, target with
                | Direct, _ -> target
                | Indirect, Control -> Control
                | _ -> tt.kind
            in
            {tt with kind}

        let to_generic tt =
            Result.Str(pp tt)
    end

module MTT = Taint.MakeMultiTag (TT)

module TBBase = 
    struct
        include Taint.MakeBank (MTT)

        let merge = MTT.merge ~tt_merge:TT.merge

        let byte_bit_merge = merge
        let bit_merge = merge

        let direct tt =
            tt

        let indirect tt =
            MTT.of_list @@ List.map (TT.relegate_kind TT.Indirect) @@ MTT.to_list tt

        let control tt =
            MTT.of_list @@ List.map (TT.relegate_kind TT.Control) @@ MTT.to_list tt

        let unary_taint _ op a t =
            match op with
            | Binsec.Dba.Unary_op.UMinus
            | Not -> t
            | Sext(size)
            | Uext(size) ->
            (
                let s = Binsec.Dba.Expr.size_of a in
                assert (size >= s);
                let ext = List.init (size - s) (fun _ -> None) in
                t @ ext
            )
            | Restrict(itv) ->
            (
                let rec aux i res = function
                    | h::t ->
                    (
                        if i >= itv.Binsec.Interval.lo && i <= itv.Binsec.Interval.hi
                        then aux (i + 1) (h::res) t
                        else aux (i + 1) res t
                    )
                    | [] -> res
                in
                assert (itv.Binsec.Interval.hi < List.length t);
                List.rev @@ aux 0 [] t
            )
            
        let binary_taint expr op a ta b tb =
            let check_length () =
                let a = List.length ta in
                let b = List.length tb in
                if not (a = b)
                then raise (Failure (Printf.sprintf "taint byte length mismatch %d %d" a b))
            in
            let mergeda = lazy (List.fold_left merge None ta) in
            let mergedb = lazy (List.fold_left merge None tb) in
            let full_merge = lazy
                (
                    let t = merge (Lazy.force mergeda) @@ Lazy.force mergedb in
                    let s = Binsec.Dba.Expr.size_of expr in
                    List.init s (fun _ -> t)
                )
            in
            let cancel = lazy (List.init (Binsec.Dba.Expr.size_of expr) (fun _ -> None)) in
            let byte_merge = lazy (List.map2 merge ta tb) in
            let is_const_null = function
                | Binsec.Dba.Expr.Cst(bv) -> Z.zero = Binsec.Bitvector.value_of bv
                | _ -> false
            in
            match op with
            | Binsec.Dba.Binary_op.Minus 
            | DivU
            | DivS
            | Eq
            | Diff
            | LeqU
            | LtU
            | GeqU
            | GtU
            | LeqS
            | LtS
            | GeqS
            | GtS ->
            (
                check_length ();
                if Binsec.Dba.Expr.is_equal a b
                then Lazy.force cancel
                else Lazy.force full_merge
            )
            | ModU
            | ModS ->
            (
                check_length ();
                if (Binsec.Dba.Expr.is_equal a b) || is_const_null b
                then Lazy.force cancel
                else Lazy.force full_merge
            )
            | Or
            | And -> Lazy.force byte_merge
            | Xor ->
            (
                check_length ();
                if Binsec.Dba.Expr.is_equal a b
                then Lazy.force cancel
                else Lazy.force byte_merge
            )
            | Concat -> ta @ tb
            | _ -> 
            (
                check_length ();
                Lazy.force full_merge
            )

        let decide instr inputs b =
            if instr.Instruction.raw_ins = "syscall " || instr.Instruction.raw_ins = "rdtscp "
            then Symbolic.DbaTaint.Fallback
            else
            (
                try
                    ignore @@ List.find (fun sto -> has_taint sto b) inputs;
                    Symbolic.DbaTaint.Propagate
                with Not_found -> Symbolic.DbaTaint.Erase
            )

        let fallback instr b =
            let tt = List.fold_left
                (
                    fun res sto ->
                        match get_taint sto b with
                        | Some(taint) -> List.fold_left (MTT.merge ~tt_merge:TT.merge) res taint
                        | _ -> res
                )
                None instr.Instruction.direct_reads
            in
            let filter = function
                | Storage.Register(reg, _) -> not (reg = "rflags")
                | _ -> true
            in
            List.fold_left
                (
                    fun b sto ->
                        if filter sto
                        then taint sto tt b
                        else taint sto None b
                )
                b instr.Instruction.writes
    end

module S =
    struct
        type t =
            {
                indirect : bool;
                control : bool;
                flags : bool;
                drop : Str.regexp;
                drop_until : Function.CallId.t option
            }

        let create ?(tag = None) () =
            {indirect = false; control = false; flags = false; drop = Str.regexp "$"; drop_until = None}

        let add_stats_to_bundle b _ = b

        let set_indirect indirect s =
            {s with indirect}

        let set_control control s =
            {s with control}

        let set_flags flags s =
            {s with flags}

        let set_drop drop s =
            {s with drop}

        let check_drop s =
            match s.drop_until with
            | Some(_) -> true
            | _ -> false

        let update_drop func s =
            match func, s.drop_until with
            | Function.CallRet(call_id, _), Some(call_id2) when call_id = call_id2 || call_id.Function.CallId.stack_id > call_id2.stack_id -> {s with drop_until = None}
            | Function.Entry({fname; call_id = Some(call_id); _}), None when Str.string_match s.drop fname 0 -> {s with drop_until = Some(call_id)}
            | _ -> s
    end

module TB = Symbolic.DbaTaint.Make (TBBase)
module A = Analysis.Make (TB) (S)

let propag_callback instr a =
    let s = A.get_state a in
    if S.check_drop s
    then A.set_bank a @@ TB.erase instr @@ A.get_bank a
    else A.set_bank a @@ TB.propagation ~propagate_flags:s.S.flags ~propagate_indirect:s.S.indirect ~propagate_control:s.S.control instr @@ A.get_bank a

let func_callback func a =
    A.set_state a @@ S.update_drop func @@ A.get_state a

let init_analysis
    ?(register_stats = true) 
    ?indirect
    ?control
    ?flags
    ?drop
    ?(tag = None)
        name =
    let indirect = indirect_opt#maybe_get ?tag indirect in
    let control = control_opt#maybe_get ?tag control in
    let flags = flag_opt#maybe_get ?tag flags in
    let drop = drop_opt#maybe_get ?tag drop in
    let a = A.create ~tag name in
    let a = A.set_state a 
        @@ S.set_indirect indirect 
        @@ S.set_control control 
        @@ S.set_flags flags
        @@ S.set_drop (Utils.pattern_list @@ String.split_on_char ';' drop)
        @@ A.get_state a in
    if register_stats
    then Stats.register ~silent:true @@ A.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ A.get_name a) a
    ;
    let todo a =
        a
            |> A.InstructionCallbacks.add "propag" propag_callback
            |> A.FunctionCallbacks.add "func" func_callback
    in
    a
        |> A.FunctionCallbacks.add "start" (A.FunctionCallbacks.start_callback ~name:"start" ~todo)

module PB =
    struct
        module A = A

        type p = A.t

        let name = "bytedep"
        let desc = "Byte dependency taint analysis."

        let init ?(tag = None) () =
            init_analysis ~tag "Byte Dependency Taint Policy"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option P.name "-indirect" (Options.TaggedOptions.Bool(indirect_opt)) "Enable indirect taint propagation (from memory address generation operands).";
    Options.register_policy_alias P.name "-indirect" "-i";
    Options.register_policy_option P.name "-control" (Options.TaggedOptions.Bool(indirect_opt)) "Enable control taint propagation (only DBA if statements).";
    Options.register_policy_alias P.name "-control" "-c";
    Options.register_policy_option P.name "-drop-taint" (Options.TaggedOptions.String(drop_opt)) "Drop taint in specified functions (format: name;<regex>;...)";
    Options.register_policy_option P.name "-flags" (Options.TaggedOptions.Bool(flag_opt)) "Enable propagation to flags."
