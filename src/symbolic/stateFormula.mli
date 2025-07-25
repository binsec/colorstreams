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

(**symbolic state SMT formulas*)

(**Constraints are represented as functions over free variables.
   For example, applying a path constraint to declared variables is done by asserting the application of the path constraint's corresponding definition to those variables.
 *)

(**inputs, i.e., non-projection free variables (e.g., from sources)*)
module Inputs :
    sig
        module Input :
            sig
                type t

                val get_id : t -> Identifier.t
                val get_name : t -> string
                val get_desc : t -> string list
                (**get input description (list of keywords, from sources)*)

                val pp : t -> string
                
                val compare : t -> t -> int
                val equals : t -> t -> bool
            end

        module InputMap : Map.S with type key = Input.t

        type 'a t

        val empty : 'a t

        val add : string -> string list -> 'a -> 'a t -> Input.t * 'a t

        val find : Input.t -> 'a t -> 'a
        val iter : (Input.t -> 'a -> unit) -> 'a t -> unit
        val fold : (Input.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    end

(**projected variables, i.e., free variables constrained to non-free variables allowing to extract the latter's value (e.g., for sinks)*)
module Projections :
    sig
        module Projection :
            sig
                type t

                val get_id : t -> Identifier.t
                val get_name : t -> string
                val get_suffix : t -> string
                val pp : t -> string

                val compare : t -> t -> int
                val equals : t -> t -> bool
            end

        module ProjMap : Map.S with type key = Projection.t

        type 'a t
        (**associates data of type ['a] to projections*)

        val empty : 'a t

        val add : ?suff:string -> string -> 'a -> 'a t -> Projection.t * 'a t
        val find : Projection.t -> 'a t -> 'a
        val find_first : (Projection.t -> bool) -> 'a t -> Projection.t * 'a
        val remove : Projection.t -> 'a t -> 'a t

        val iter : (Projection.t -> 'a -> unit) -> 'a t -> unit
        val fold : (Projection.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val update : ('a ProjMap.t -> 'b ProjMap.t) -> 'a t -> 'b t
    end

module StatusVal :
    sig
        type t = Sat of (Binsec.Formula.bv_term * Z.t) list | Unsat | Error
    end

module SolverStats :
    sig
        type t =
            {
                z3_cnt : Stats.counter;
                bitw_cnt : Stats.counter;
                q3b_cnt : Stats.counter;
                (*q3b_pbdd_cnt : Stats.counter;*)
                sat_cnt : Stats.counter;
                unsat_cnt : Stats.counter;
                err_cnt : Stats.counter;
                timer : Stats.timer;
                b : Stats.Bundle.t
            }

        val create : ?report:bool -> string -> t
        val incrres : StatusVal.t -> t -> unit
        val incrsolv : string -> t -> unit
    end

type constr_builder = Binsec.Formula.var -> Binsec.Formula.term list -> Binsec.Formula.bl_term
(**variable-agnostic constraints (can be transferred between variables)*)

(**formula manipulation utilities*)
module FormulaUtils :
    sig
        val clone_var : ?name:string -> Binsec.Formula.var -> Binsec.Formula.var
        (**creates a copy of a variable with a new name*)

        val clone_decl : ?name:string -> Binsec.Formula.decl -> Binsec.Formula.decl
        (**creates a copy of a declaration statement with a new name*)

        val var_to_decl : Binsec.Formula.var -> Binsec.Formula.sort list -> Binsec.Formula.decl
        (**creates a variable declaration*)

        val decl_to_term : Binsec.Formula.decl -> Binsec.Formula.term list -> Binsec.Formula.term
        (**creates a term based on a declaration*)

        val def_to_term : Binsec.Formula.def -> Binsec.Formula.term list -> Binsec.Formula.term
        (**creates a term based on a definition*)

        val def_var_args : Binsec.Formula.def -> Binsec.Formula.var * Binsec.Formula.decl list
        (**get the arguments of a defined variable as variables and declarations*)

        val interval : Interval.t -> Binsec.Formula.bv_term -> Binsec.Formula.bl_term
        (**creates a value interval constraint*)

        val get_bl : Binsec.Formula.term -> Binsec.Formula.bl_term
        val get_bv : Binsec.Formula.term -> Binsec.Formula.bv_term
        val get_ax : Binsec.Formula.term -> Binsec.Formula.ax_term

        val app_bl : (Binsec.Formula.term -> Binsec.Formula.term) -> Binsec.Formula.bl_term -> Binsec.Formula.bl_term
        val app_bv : (Binsec.Formula.term -> Binsec.Formula.term) -> Binsec.Formula.bv_term -> Binsec.Formula.bv_term
        val app_ax : (Binsec.Formula.term -> Binsec.Formula.term) -> Binsec.Formula.ax_term -> Binsec.Formula.ax_term

        module Quant :
            sig
                type quant = Exists | Forall | NotExists | NotForall
                type quantdesc = quant * Binsec.Formula.decl list
                type quantbody = Body of Binsec.Formula.bl_term | NestedQuant of quantdesc * quantbody
                type quantentry = quantdesc * quantbody

                type Binsec.Formula.custom_desc += Quant of quantentry

                val mk_quant : quant -> Binsec.Formula.decl list -> quantbody -> quantentry
                val mk_quant_exists : Binsec.Formula.decl list -> Binsec.Formula.bl_term -> quantentry
                val mk_quant_forall : Binsec.Formula.decl list -> Binsec.Formula.bl_term -> quantentry
                val mk_quant_notexists : Binsec.Formula.decl list -> Binsec.Formula.bl_term -> quantentry
                val mk_quant_notforall : Binsec.Formula.decl list -> Binsec.Formula.bl_term -> quantentry

                val to_entry : quantentry -> Binsec.Formula.entry
            end

        module CustomCmd :
            sig
                module type Sig =
                    sig
                        type Binsec.Formula.custom_desc += Cmd of Binsec.Formula.term

                        val mk_cmd : Binsec.Formula.term -> Binsec.Formula.entry
                    end

                val make : string -> (module Sig)

                module Minimize : Sig
                module Maximize : Sig
            end
    end

type t

module Status : Result.CustomResult with type value = StatusVal.t
(**basic result for solver queries*)

(**portfolio of solver*)
module MultiSolver :
    sig
        type sat = Cadical | CMS | Lingeling | Minisat | Picosat | Kissat | Gimsatul
        type smt = Z3 | OldBitwuzla of sat | Q3B | Q3B_pBDD | Bitwuzla
    end

val create : ?inputs:('a Inputs.t) -> ?projections:('a Projections.t) -> Binsec.Formula.formula -> t
(**[create ~projections fml] creates a state formula from a binsec formula with the provided projections.
Non-projection free variables are attributed to the provided inputs.

Also performs formula optimization.
 *)

val compile : t -> Binsec.Formula.formula
(**compile a state formula back into a binsec formula*)

val clean : t -> t
(**remove all custom constraints, variables etc...*)

val pp : t -> string
val pp_base : t -> string
(**prints the original optimized binsec formula*)

val get_base : t -> Binsec.Formula.formula
(**returns the original optimized binsec formula*)

val get_free_variables : t -> Binsec.Formula.var list
(**get a list of all free variables (inputs + projections)*)

val get_inputs : t -> (int * Binsec.Formula.var) Inputs.t
(**get inputs with the associated free variables and their index*)

val get_declarations : t -> Binsec.Formula.decl list
(**get a list of all declarations*)

val get_projections : t -> Binsec.Formula.def Projections.t
(**get the definitions corresponding to projections*)

val get_definitions : t -> Binsec.Formula.def list
(**get a list of all definitions*)

val get_path_constraint : t -> Binsec.Formula.def
(**get the definition corresponding to the overall path constraint*)

val set_quant : bool -> t -> t
(**set whether the formula has quantifiers*)

val set_solver : MultiSolver.smt option -> t -> t
(**select a single applicable solver*)

val has_quant : t -> bool
val has_free_array : t -> bool

val add_declare : Binsec.Formula.decl -> t -> t
(**add a custom declaration*)

val add_define : Binsec.Formula.def -> t -> t
(**add a custom definition*)

val add_assert : Binsec.Formula.bl_term -> t -> t
(**add a custom assert*)

val add_custom : Binsec.Formula.entry -> t -> t
(**add a custom entry*)

val add_proj_constr : Projections.Projection.t -> constr_builder -> t -> t
(**add a projection constraint*)

val get_proj_constr : Projections.Projection.t -> t -> constr_builder
(**get the constraints on a given projection*)

val remove_proj_constr : Projections.Projection.t -> t -> t
(**remove all constraints on a projection*)

val add_proj_restrict : Projections.Projection.t -> lo:int -> hi:int -> t -> Projections.Projection.t * t
(**creates a new projection as bytes lo to hi from a given one*)

val check_sat : ?get:(Binsec.Formula.bv_term list) -> t -> Status.result
(**[check_sat ~get sf] checks the satisfiability of [sf]

[get] is a list of free variable terms to get a model for
*)

val from_file : File.t -> t
(**loads an SMT formula from a file*)
