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

(**disassembly of opcodes into DBA instructions*)

module DisasmStats :
    sig
        val register : unit -> unit
    end

type t = Binsec.Instruction.t

type exn += DbaFailure of string

val of_instruction : ?base:Address.t -> Instruction.t -> t
(**[of_instruction ~base instr] disassembles [instr] into DBA instructions

[base] sets the instruction's address
 *)

val pp : t -> string

val mapi : f:(int -> Binsec.Dba.Instr.t -> Binsec.Dba.Instr.t) -> t -> t

(**DBA instruction visitor*)
module Visitor :
    sig
        class virtual visitor :
            object
                method visit_expr_before : Binsec.Dba.Expr.t -> Binsec.Dba.Expr.t
                method visit_expr_after : Binsec.Dba.Expr.t -> Binsec.Dba.Expr.t

                method visit_lv_before : Binsec.Dba.LValue.t -> Binsec.Dba.LValue.t
                method visit_lv_after : Binsec.Dba.LValue.t -> Binsec.Dba.LValue.t

                method visit_instr_before : Binsec.Dba.Instr.t -> Binsec.Dba.Instr.t
                method visit_instr_after : Binsec.Dba.Instr.t -> Binsec.Dba.Instr.t
            end

        val visit_expr : visitor -> Binsec.Dba.Expr.t -> Binsec.Dba.Expr.t
        val visit_lv : visitor -> Binsec.Dba.LValue.t -> Binsec.Dba.LValue.t
        val visit_instr : visitor -> Binsec.Dba.Instr.t -> Binsec.Dba.Instr.t
        val visit : visitor -> t -> t
    end
