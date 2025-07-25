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

module Dhunk =
    struct
        let get i =
            Binsec.Instruction.hunk i

        let pp hunk =
            Binsec.Dhunk.pp Format.str_formatter hunk;
            Format.flush_str_formatter ()

        class stop_state_getter =
            object (self)
                inherit Binsec.Dba_visitor.dba_inplace_visitor

                val mutable result = None

                method! visit_stop state =
                    result <- state

                method get_result =
                    result
            end

        let check_state hunk =
            let check i =
                let ssg = new stop_state_getter in
                ssg#visit_instrkind i;
                match ssg#get_result with
                | Some(Binsec.Dba.KO) -> raise (Failure "KO")
                | Some(Binsec.Dba.Undecoded _) -> raise (Failure "Undecoded")
                | Some(Binsec.Dba.Unsupported _) -> raise (Failure "Unsupported")
                | _ -> ()
            in
            Binsec.Dhunk.iter ~f:check hunk
    end

module Disasm =
    struct
        let decode ?(base = Address.zero) instr =
            let base_va = Binsec.Virtual_address.of_bigint base in
            let i, _ = Binsec.Binstream.of_nibbles instr
                |> Binsec.Disasm_core.decode_binstream ~base:base_va
            in
            Dhunk.get i |> Dhunk.check_state;
            i

        let decode_hunk instr =
            decode instr
                |> Dhunk.get
    end

module DisasmStats =
    struct
        let tot = new Stats.counter "tot" "Queries" 0
        let succ = new Stats.counter "succ" "Success" 0
        let err = new Stats.counter "err" "Error" 0
        let cache = new Stats.counter "cache" "Cache hits" 0
        let runtime = new Stats.timer "runtime" "Disassembler runtime"

        let b = Stats.Bundle.create "Disassembler"
            |> Stats.Bundle.add_stat (tot :> Stats.stat)
            |> Stats.Bundle.add_stat (succ :> Stats.stat)
            |> Stats.Bundle.add_stat (err :> Stats.stat)
            |> Stats.Bundle.add_stat (cache :> Stats.stat)
            |> Stats.Bundle.add_stat (runtime :> Stats.stat)

        let registered = ref false

        let register () =
            if not !registered
            then
            (
                Stats.register ~big:false b;
                registered := true
            )
    end

type t = Binsec.Instruction.t

type cache_entry = OK of t | KO of string

let cache = ref Utils.ZMap.empty

type exn += DbaFailure of string

let of_instruction ?(base = Address.zero) instr =
    DisasmStats.register ();
    DisasmStats.tot#incr;
    DisasmStats.runtime#start;
    let res = 
        let byte_instr = Z.of_string ("0x" ^ instr.Instruction.raw_bytes) in
        try
            let dba = 
                match Utils.ZMap.find byte_instr !cache with
                | OK(dba) -> dba
                | KO(msg) -> raise (DbaFailure(msg))
            in
            DisasmStats.cache#incr;
            dba
        with Not_found ->
        (
            try
                let dba = Disasm.decode ~base instr.Instruction.raw_bytes in
                DisasmStats.succ#incr;
                cache := Utils.ZMap.add byte_instr (OK(dba)) !cache;
                dba
            with e ->
            (
                DisasmStats.err#incr;
                DisasmStats.runtime#stop;
                let msg = Printf.sprintf "%s\ncould not convert <%s> (%s) to dba (%s)"
                    (Instruction.pp instr)
                    instr.raw_ins
                    instr.raw_bytes
                    (Printexc.to_string e)
                in
                Message.Wrap.send (Message.Wrap.Warning(lazy msg));
                cache := Utils.ZMap.add byte_instr (KO(msg)) !cache;
                raise (DbaFailure msg)
            )
        )
    in
    DisasmStats.runtime#stop;
    res

let pp instr =
    Binsec.Instruction.pp Format.str_formatter instr;
    Format.flush_str_formatter ()

let mapi ~f i =
    let address = Binsec.Instruction.address i in
    let size = Binsec.Instruction.size i in
    let opcode = Binsec.Instruction.opcode i in
    let mnemonic = Binsec.Instruction.mnemonic i in
    let dhunk = Binsec.Dhunk.mapi ~f @@ Dhunk.get i in
    Binsec.Instruction.create address size opcode mnemonic dhunk

module Visitor =
    struct
        class virtual visitor =
            object(self)
                method visit_expr_before e:Binsec.Dba.Expr.t = e
                method visit_expr_after e:Binsec.Dba.Expr.t = e

                method visit_lv_before lv:Binsec.Dba.LValue.t = lv
                method visit_lv_after lv:Binsec.Dba.LValue.t = lv

                method visit_instr_before i:Binsec.Dba.Instr.t = i
                method visit_instr_after i:Binsec.Dba.Instr.t = i
            end

        let rec visit_expr v e =
            let e = v#visit_expr_before e in
            let e = 
                match e with
                | Binsec.Dba.Expr.Load(s, d, e, array) -> Binsec.Dba.Expr.load ?array (Binsec.Size.Byte.unsafe_of_bits (s * 8)) d @@ visit_expr v e
                | Unary(op, e) -> Binsec.Dba.Expr.unary op @@ visit_expr v e
                | Binary(op, e, f) -> Binsec.Dba.Expr.binary op (visit_expr v e) @@ visit_expr v f
                | Ite(e, f, g) -> Binsec.Dba.Expr.ite (visit_expr v e) (visit_expr v f) @@ visit_expr v g
                | _ -> e
            in
            v#visit_expr_after e

        let visit_lv v lv =
            let lv = v#visit_lv_before lv in
            let lv =
                match lv with
                | Binsec.Dba.LValue.Store(s, d, e, array) -> Binsec.Dba.LValue.store ?array (Binsec.Size.Byte.unsafe_of_bits (s * 8)) d @@ visit_expr v e
                | _ -> lv
            in
            v#visit_lv_after lv

        let visit_instr v i =
            let i = v#visit_instr_before i in
            let i = 
                match i with
                | Binsec.Dba.Instr.Assign(lv, e, id) -> Binsec.Dba.Instr.assign (visit_lv v lv) (visit_expr v e) id
                | DJump(e, tag) -> Binsec.Dba.Instr.dynamic_jump ~tag @@ visit_expr v e
                | If(e, a, b) -> Binsec.Dba.Instr.ite (visit_expr v e) a b
                | Assert(e, id) -> Binsec.Dba.Instr._assert (visit_expr v e) id
                | Assume(e, id) -> Binsec.Dba.Instr.assume (visit_expr v e) id
                | Nondet(lv, id) -> Binsec.Dba.Instr.non_deterministic (visit_lv v lv) id
                | Undef(lv, id) -> Binsec.Dba.Instr.undefined (visit_lv v lv) id
                | _ -> i
            in
            v#visit_instr_after i

        let visit (v : visitor) i =
            mapi ~f:(fun _ i -> visit_instr v i) i
    end
