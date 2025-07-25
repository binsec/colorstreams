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

module Operand =
    struct
        type register =
            {
                name : string;
                size : int;
                full : register option;
            }

        type 'a memcons =
            {
                addr : 'a;
                size : int;
                segment : register option;
                base : register option;
                shift : int;
                index : register option;
                scale : int;
            }

        type memory = Address.t memcons

        type 'a opcons = Register of register | Memory of 'a
        type rw = Read | Write | Both
        type comptype = Segment | Base | Index
        type 'a kindcons = Explicit of int | Implicit | Component of comptype * 'a * rw

        type op = memory opcons
        type kind = op kindcons

        type t =
            {
                op : op;
                rw : rw;
                kind : kind;
            }

        let size op =
            match op.op with
            | Register(reg) -> reg.size
            | Memory(mem) -> mem.size

        let register_to_sto full_regs reg =
            if full_regs
            then
            (
                match reg.full with
                | Some(reg) -> Storage.Register(reg.name, reg.size)
                | _ -> Storage.Register(reg.name, reg.size)
            )
            else Storage.Register(reg.name, reg.size)

        let to_storage ?(full_regs = true) op =
            match op.op with
            | Register(reg) -> register_to_sto full_regs reg
            | Memory(mem) -> Storage.Memory(mem.addr, mem.size)

        let is_storage ?(full_regs = true) op sto =
            let opsto = to_storage ~full_regs op in
            opsto = sto

        let mem_expr mem =
            let maybe f g h =
                match g with
                | Some(g) -> f (register_to_sto true g) h
                | _ -> h
            in
            Expr.Const(Z.of_int mem.shift, 8)
                |> maybe (fun sto e -> Expr.Bnop(Expr.Add, Expr.Var(sto), e)) mem.base
                |> maybe (fun sto e -> Expr.Bnop(Expr.Add, Expr.Var(sto), e)) mem.segment
                |> maybe (fun sto e -> Expr.Bnop(Expr.Add, e, Expr.Bnop(Expr.Mul, Expr.Var(sto), Expr.Const(Z.of_int mem.scale, 8)))) mem.index

        let pp op =
            let pp_reg reg =
                match reg.full with
                | Some(full) -> Printf.sprintf "%s(%s)[%d(%d)]" reg.name full.name reg.size full.size
                | _ -> Printf.sprintf "%s[%d]" reg.name reg.size
            in
            match op.op with
            | Register(reg) -> pp_reg reg
            | Memory(mem) ->
            (
                let rec aux res = function
                    | ""::toks, _::seps -> aux res (toks, seps)
                    | tok::toks, sep::seps -> 
                    (
                        if res = ""
                        then aux tok (toks, seps)
                        else aux (res ^ sep ^ tok) (toks, seps)
                    )
                    | _ -> res
                in
                let maybe f = function
                    | Some(reg) -> f reg
                    | _ -> ""
                in
                Printf.sprintf "%s<%s>[%d]"
                    (Address.to_string_hex mem.addr)
                    (aux "" 
                        (
                            [
                                maybe pp_reg mem.segment; 
                                maybe pp_reg mem.base;
                                string_of_int mem.shift;
                                maybe pp_reg mem.index;
                                maybe (fun _ -> string_of_int mem.scale) mem.index
                            ],
                            [
                                "";
                                "+";
                                "+";
                                "+";
                                "*"
                            ]
                        )
                    )
                    @@ mem.size
            )
    end

type t =
    {
        branch : bool option;
        fname : string;
        address : Address.t;
        raw_bytes : string;
        raw_ins : string;
        operands : Operand.t list;
        reads : Storage.t list;
        direct_reads : Storage.t list;
        writes : Storage.t list;
    }

module Template =
    struct
        type addrcons = Address.t -> Address.t -> Address.t -> Address.t
        type memory = addrcons Operand.memcons
        type op = memory Operand.opcons
        type kind = op Operand.kindcons

        type operand =
            {
                op : op;
                rw : Operand.rw;
                kind : kind;
            }

        let parse_operand s : operand option =
            try
                let opexp = Str.regexp {|\(\([0-9]+\):\)?\([RW]+\)\(REG\|MEM\|IMM\)<\([^>]*\)>\((\([^)]*\))\)?\(\[\([0-9:]+\)\]\)?|} in
                if s = "" then None
                else if Str.string_match opexp s 0
                then
                (
                    let kind = 
                        try
                            Operand.Explicit(int_of_string @@ Str.matched_group 2 s)
                        with Not_found -> Implicit
                    in
                    let rw = 
                        match Str.matched_group 3 s with
                        | "R" -> Operand.Read
                        | "W" -> Write
                        | "RW" -> Both
                        | _ -> assert false
                    in
                    let pref = Str.matched_group 4 s in
                    let desc = Str.matched_group 5 s in
                    let parse_reg sizes names =
                        match List.map int_of_string sizes, names with
                        | [size; fullsize], [name; fullname] ->
                        (
                            let full =
                                if size = fullsize
                                then None
                                else
                                (
                                    let avxexp = Str.regexp {|zmm\([0-9]+\)|} in
                                    if Str.string_match avxexp fullname 0
                                    then
                                    (
                                        let name = "ymm" ^ (Str.matched_group 1 fullname) in
                                        Some({Operand.name; size = 32; full = None})
                                    )
                                    else Some({name = fullname; size = fullsize; full = None})
                                )
                            in
                            {Operand.name; size; full}
                        )
                        | _ -> assert false
                    in
                    match pref with
                    | "REG" -> 
                    (
                        let sizes = String.split_on_char ':' @@ Str.matched_group 9 s in
                        let names = String.split_on_char ':' desc in
                        Some({op = Register(parse_reg sizes names); rw; kind})
                    )
                    | "MEM" ->
                    (
                        let size = int_of_string @@ Str.matched_group 9 s in
                        let regexp = {|REG<\([A-Za-z0-9:]+\)>\[\([0-9:]+\)\]|} in
                        let descexp = Str.regexp (regexp ^ {|\+|} ^ regexp ^ {|\+\([-0-9]+\)\+|} ^ regexp ^ {|\*\([0-9]+\)|}) in
                        let addrdesc = Str.matched_group 7 s in
                        assert (Str.string_match descexp addrdesc 0);
                        let maybe_reg names sizes =
                            let sizes = String.split_on_char ':' @@ Str.matched_group sizes addrdesc in
                            let names = String.split_on_char ':' @@ Str.matched_group names addrdesc in
                            if names = ["invalid"; "invalid"]
                            then None
                            else Some(parse_reg sizes names)
                        in
                        let segment = maybe_reg 1 2 in
                        let base = maybe_reg 3 4 in
                        let shift = int_of_string @@ Str.matched_group 5 addrdesc in
                        let index = maybe_reg 6 7 in
                        let scale = int_of_string @@ Str.matched_group 8 addrdesc in
                        let addr read1 read2 write =
                            match desc with
                            | "$0" -> read1
                            | "$1" -> read2
                            | "$2" -> write
                            | _ -> Address.of_string desc
                        in
                        Some({op = Memory({addr : addrcons; size; segment; base; shift; index; scale} : memory); rw; kind} : operand) 
                    )
                    | "IMM" -> None
                    | _ -> assert false
                )
                else assert false
            with e -> raise (Failure (Printf.sprintf "Error: Could not parse operand from <%s> (%s)." s @@ Printexc.to_string e))

        let to_operand read1 read2 write (tpl : operand) : Operand.t =
            let get_op op =
                match op with
                | Operand.Memory(mem) -> 
                (
                    let addr = mem.Operand.addr read1 read2 write in
                    Operand.Memory({Operand.addr; size = mem.size; segment = mem.segment; base = mem.base; shift = mem.shift; index = mem.index; scale = mem.scale})
                )
                | Register(reg) -> Operand.Register(reg)
            in
            let op = get_op tpl.op in
            let kind =
                match tpl.kind with
                | Explicit(n) -> Operand.Explicit(n)
                | Implicit -> Operand.Implicit
                | Component(comptype, op, rw) -> Operand.Component(comptype, get_op op, rw)
            in
            {op; rw = tpl.rw; kind}

        let parse line =
            try
                let fname, address, raw_bytes, raw_ins, branch, pred, raw_io =
                    match String.split_on_char ';' line with
                    | fname::addr::ins_bytes::ins::branch::pred::l -> 
                    (
                        let io =
                            match l with
                            | [io] -> io
                            | [] -> ""
                            | _ -> assert false
                        in
                        let branch = branch = "B" in
                        let pred = pred = "P" in
                        fname, Address.of_string addr, ins_bytes, ins, branch, pred, io
                    )
                    | _ -> assert false
                in
                let (operands : operand list) = 
                    if raw_ins = "vzeroupper" 
                    then 
                    (
                        let mk_ymm n =
                            let name = Printf.sprintf "ymm%d" n in
                            {op = Register({name; size = 32; full = None}); rw = Write; kind = Implicit}
                        in
                        List.init 16 mk_ymm
                    )
                    else if raw_ins = "syscall "
                    then [{op = Register({name = "rax"; size = 8; full = None}); rw = Write; kind = Implicit}]
                    else
                    (
                        let direct_ops = List.filter_map parse_operand @@ String.split_on_char ' ' raw_io in
                        List.fold_left
                            (
                                fun res op ->
                                    match op.op with
                                    | Memory(mem) ->
                                    (
                                        let maybe_comp comp comptype l =
                                            match comp with
                                            | Some(reg) -> {op = Register(reg); rw = Read; kind = Component(comptype, op.op, op.rw)}::l
                                            | _ -> l
                                        in
                                        let indirect = []
                                            |> maybe_comp mem.segment Segment 
                                            |> maybe_comp mem.base Base
                                            |> maybe_comp mem.index Index
                                        in
                                        res @ indirect
                                    )
                                    | _ -> res
                            )
                            direct_ops direct_ops
                    )
                in
                Z.of_string ("0x" ^ raw_bytes),
                branch,
                pred,
                fun fname address branch read1 read2 write ->
                    let operands = List.map (to_operand read1 read2 write) operands in
                    let reads, direct_reads, writes = List.fold_left 
                        (
                            fun (reads, direct_reads, writes) op ->
                                let sto = Operand.to_storage op in
                                match op.kind, op.rw with
                                | Component(_), Read -> sto::reads, direct_reads, writes
                                | _, Read -> sto::reads, sto::direct_reads, writes
                                | _, Write -> reads, direct_reads, sto::writes
                                | Component(_), Both -> sto::reads, direct_reads, sto::writes
                                | _, Both -> sto::reads, sto::direct_reads, sto::writes
                        )
                        ([], [], []) operands
                    in
                    let reads = List.sort_uniq Storage.compare reads in
                    let direct_reads = List.sort_uniq Storage.compare direct_reads in
                    let writes = List.sort_uniq Storage.compare writes in
                    {branch; fname; address; raw_ins; raw_bytes; operands; reads; direct_reads; writes}
            with e -> raise (Failure (Printf.sprintf "could not parse instruction template from <%s> (%s)" line @@ Printexc.to_string e))
            
        let cache = ref Utils.ZMap.empty

        let dynopexpr = Str.regexp {|\$[012]|}

        let register_template line =
            let opcode, branch, pred, tpl = parse line in
            let has_dyn_op =
                try
                    ignore @@ Str.search_forward dynopexpr line 0;
                    true
                with Not_found -> false
            in
            cache := Utils.ZMap.add opcode (branch, pred, has_dyn_op, tpl) !cache;
            Message.Wrap.send (Message.Wrap.Debug("INSTRUCTION", lazy (Printf.sprintf "added template for opcode %s" @@ Z.format "%#x" opcode)))

        let get_template opcode =
            Utils.ZMap.find opcode !cache

        let has_template opcode =
            Utils.ZMap.mem opcode !cache
    end

let has_template = Template.has_template

let get opcode =
    try
        Template.get_template opcode
    with Not_found -> raise (Failure(Printf.sprintf "No template found for instruction with opcode %s" @@ Address.to_string_hex opcode))

let create func addr opcode branch read1 read2 write =
    let _, _, _, tpl = get opcode in
    tpl func addr branch read1 read2 write

let parse func line =
    match String.split_on_char ' ' line with
    | [addr; opcode; branch; exec; read1; read2; write] ->
    (
        let addr = Address.of_string addr in
        let opcode = Z.of_string ("0x" ^ opcode) in
        let branch =
            match branch with
            | "NA" -> None
            | "NB" -> Some(false)
            | "B" -> Some(true)
            | _ -> assert false
        in
        let exec =
            match exec with
            | "NE" -> false
            | "E" | "NA" -> true
            | _ -> assert false
        in
        if exec
        then
        (
            let read1 = Address.of_string read1 in
            let read2 = Address.of_string read2 in
            let write = Address.of_string write in
            Some(create func addr opcode branch read1 read2 write)
        )
        else None
    )
    | _ -> assert false

let create_from_nothing func addr opcode =
    let branch, pred, has_dyn_op, tpl = get opcode in
    if branch || pred || has_dyn_op then None else Some(tpl func addr None Z.zero Z.zero Z.zero)

let add_template = Template.register_template

let pp i =
    let print_op read res op =
        if
            match op.Operand.rw with
            | Both -> true
            | Read -> read
            | Write -> not read
        then res ^ (Operand.pp op) ^ ", "
        else res
    in
    (Printf.sprintf "%s : %s : %s" (Address.to_string_hex i.address) i.fname i.raw_ins) ^
    (
        match i.branch with
        | Some(true) -> " (B)\n"
        | Some(false) -> " (NB)\n"
        | _ -> "\n"
    ) ^
    (Printf.sprintf "├─R: %s\n" (List.fold_left (print_op true) "" i.operands)) ^
    (Printf.sprintf "└─W: %s" (List.fold_left (print_op false) "" i.operands))

let get_nth_operand n i =
    List.find 
        (
            fun op -> 
                match op.Operand.kind with
                | Explicit(m) -> n = m
                | _ -> false
        )
        i.operands
