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

(**instruction representation*)

(**{1 Instruction operands}*)

module Operand :
    sig
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
        type 'a kindcons = 
            | Explicit of int (**operand specified in the opcode*)
            | Implicit (**operand implicit to the opcode*)
            | Component of comptype * 'a * rw (**operand which is part of a memory operand*)

        type op = memory opcons
        type kind = op kindcons

        type t =
            {
                op : op;
                rw : rw;
                kind : kind;
            }

        val size : t -> int
        val to_storage : ?full_regs:bool -> t -> Storage.t

        val is_storage : ?full_regs:bool -> t -> Storage.t -> bool
        (**checks whether a storage corresponds to an operand*)

        val mem_expr : memory -> Expr.t
        val pp : t -> string
    end

(**{1 Instructions}*)

type t =
    {
        branch : bool option; (**whether the execution branches, for branching instructions*)
        fname : string; (**parent function's name*)
        address : Address.t;
        raw_bytes : string; (**raw opcode*)
        raw_ins : string; (**raw instruction*)
        operands : Operand.t list;
        reads : Storage.t list; (**read locations*)
        direct_reads : Storage.t list; (**excludes read locations corresponding to memory address components*)
        writes : Storage.t list; (**written locations*)
    }

val create : string -> Address.t -> Z.t -> bool option -> Z.t -> Z.t -> Z.t -> t
(**[create func addr opcode branch read1 read2 write] creates an instruction from a memorized template with the supplied dynamic data.

- [func] is the current function
- [addr] is the address of the instruction
- [opcode] is the opcode of the instruction (used to load the template from the cache)
- [branch] is either [None] if the instruction is not a branching instruction, [Some(false)] if the instruction does not branch or [Some(true)] if the instruction branches
- [read1] is the first read address (0 if not applicable)
- [read2] is the second read address (0 if not applicable)
- [write] is the write address (0 if not applicable)
 *)

val parse : string -> string -> t option

val create_from_nothing : string -> Address.t -> Z.t -> t option
(**Attempt to create an instruction from just function, address and opcode.*)

val add_template : string -> unit
val has_template : Z.t -> bool
val pp : t -> string

val get_nth_operand : int -> t -> Operand.t
(**[get_nth_operand n i] returns the nth explicit operand from [i]*)
