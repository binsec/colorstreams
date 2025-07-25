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

open Binsec
open Formula
open Formula_utils

module BlVarMap = Map.Make
    (struct
      type t = bl_var
      let compare a b = compare a.bl_name b.bl_name
    end)

module BvVarMap = Map.Make
    (struct
      type t = bv_var
      let compare a b = compare (a.bv_name, a.bv_size) (b.bv_name, b.bv_size)
    end)

module AxVarMap = Map.Make
    (struct
      type t = ax_var
      let compare a b = compare (a.ax_name, a.idx_size, a.elt_size) (b.ax_name, b.idx_size, b.elt_size)
    end)

module BindMap =
struct

  type ('bl, 'bv, 'ax) t = {
    bl_bind : 'bl BlVarMap.t;
    bv_bind : 'bv BvVarMap.t;
    ax_bind : 'ax AxVarMap.t;
  }

  let empty = {
    bl_bind = BlVarMap.empty;
    bv_bind = BvVarMap.empty;
    ax_bind = AxVarMap.empty;
  }

  let bl_lookup t v = try Some(BlVarMap.find v t.bl_bind) with Not_found -> None
  let bv_lookup t v = try Some(BvVarMap.find v t.bv_bind) with Not_found -> None
  let ax_lookup t v = try Some(AxVarMap.find v t.ax_bind) with Not_found -> None

  let bl_store t v x = {t with bl_bind = BlVarMap.add v x t.bl_bind;}
  let bv_store t v x = {t with bv_bind = BvVarMap.add v x t.bv_bind;}
  let ax_store t v x = {t with ax_bind = AxVarMap.add v x t.ax_bind;}

  let bl_del t v = {t with bl_bind = BlVarMap.remove v t.bl_bind; }
  let bv_del t v = {t with bv_bind = BvVarMap.remove v t.bv_bind; }
  let ax_del t v = {t with ax_bind = AxVarMap.remove v t.ax_bind; }

  let decl_store t decl x = match decl.decl_desc with
    | BlDecl(v, _) -> bl_store t v x
    | BvDecl(v, _) -> bv_store t v x
    | AxDecl(v, _) -> ax_store t v x

  let decl_del t decl = match decl.decl_desc with
    | BlDecl(v, _) -> bl_del t v
    | BvDecl(v, _) -> bv_del t v
    | AxDecl(v, _) -> ax_del t v
end


(* Remove arrays *)

module RemoveArrays =
struct
  type env = {
    bindmap  : (unit, unit, ax_term) BindMap.t; (** for arrays: None if declared, or its infos if defined *)
    base_arrays: (bv_term*bv_var) list AxVarMap.t; (** list of indices read in $key, along with the symbolic value read *)
  }

  let empty = {
    bindmap  = BindMap.empty;
    base_arrays = AxVarMap.empty;
  }

  (* check the invariants: only global bindings for arrays *)
  let no_ax_def bn =
      List.for_all (fun def -> match def.def_desc with AxDef _ -> false | _ -> true) bn
  let no_ax_decl ls =
      List.for_all (fun decl -> match decl.decl_desc with AxDecl _ -> false | _ -> true) ls

  let fresh_ctr = ref 0
  let fresh ax =
    fresh_ctr := !fresh_ctr + 1;
    let name = Format.asprintf "__array_symbolic_%s_%d" (var_name (AxVar ax)) (!fresh_ctr) in
    bv_var name 8

  (* visit functions return the modified term, and for array ones, the base *)


  let rec visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true, env
    | BlFalse -> mk_bl_false, env
    | BlFun (v,ls) -> mk_bl_fun v ls, env
    | BlLet (bn,bl) ->
      assert (no_ax_def bn);
      let bl, env = visit_bl_term env bl in
      mk_bl_let bn bl, env
    | BlUnop (u,bl) ->
      let bl, env = visit_bl_term env bl in
      mk_bl_unop u bl, env
    | BlBnop (b,bl1,bl2) ->
      let bl1, env = visit_bl_term env bl1 in
      let bl2, env = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2, env
    | BlComp (c,bl1,bl2) ->
      let bl1, env = visit_bl_term env bl1 in
      let bl2, env = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2, env
    | BvComp (c,bv1,bv2) ->
      let bv1, env = visit_bv_term env bv1 in
      let bv2, env = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2, env
    | AxComp (c,ax1,ax2) ->
      let ax1, env = visit_ax_term env ax1 in
      let ax2, env = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2, env
    | BlIte (bl,bl1,bl2) ->
      let bl, env = visit_bl_term env bl in
      let bl1, env = visit_bl_term env bl1 in
      let bl2, env = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2, env

  and resolve_select env n ax idx =
    if n==1 then resolve_select_byte env ax idx
    else
        let next, env = resolve_select_byte env ax (mk_bv_add_int idx (n-1)) in
        let base, env = resolve_select env (n-1) ax idx in
        mk_bv_concat next base, env

  and resolve_select_byte env ax idx =
    match ax.ax_term_desc with
      Formula.AxFun (v, []) -> begin match BindMap.ax_lookup env.bindmap v with
      | Some(ax) -> resolve_select_byte env ax idx
      | None -> (* symblolic array *)
        let old = match AxVarMap.find_opt v env.base_arrays with
          | None -> []
          | Some(x) -> x
        in
        let symbolic_read, new_ = match List.assoc_opt idx old with
          | None ->
            let bv = fresh v in
            bv, (idx, bv)::old
          | Some x -> x, old
        in
        let res = List.fold_left (fun t (idx', bv') -> mk_bv_ite (mk_bv_equal idx idx') (mk_bv_var bv') t) (mk_bv_var symbolic_read) old in
        let env = {env with base_arrays = AxVarMap.add v new_ env.base_arrays; } in
        res, env
      end

    | Formula.AxFun (v, _::_) -> Formula_options.Logger.fatal "remove arrays on function %a" Formula_pp.pp_ax_term ax
    | Formula.AxLet (_, _) -> Formula_options.Logger.fatal "unimplemented: remove arrays on ax let"
    | Formula.AxIte (c, a, b) ->
      let sa, env = resolve_select_byte env a idx in
      let sb, env = resolve_select_byte env b idx in
      mk_bv_ite c sa sb, env
    | Formula.Store (n, ax', idx', bv) ->
        if n == 1 then
          let base, env = resolve_select_byte env ax' idx in
          mk_bv_ite (mk_bv_equal idx idx') bv base, env
        else
          let last_idx = mk_bv_add_int idx' (n-1) in
          let bytesize = Basic_types.Constants.bytesize |> Natural.to_int in
          let base, env = resolve_select_byte env (mk_store (n-1) ax' idx' (mk_bv_extract Interval.{ lo = 0; hi = (n-1)*bytesize -1} bv)) idx in
          mk_bv_ite (mk_bv_equal idx last_idx) (mk_bv_extract Interval.{lo = (n-1)*bytesize; hi = n*bytesize-1;} bv) base, env

  and visit_bv_term env bvterm = match bvterm.bv_term_desc with
    | BvCst bv -> mk_bv_cst bv, env
    | BvFun (v,ls) -> mk_bv_fun v ls, env
    | BvLet (bn,bv) ->
      assert (no_ax_def bn);
      let bv, env = visit_bv_term env bv in
      mk_bv_let bn bv, env

    | BvUnop (u,bv) ->
      let bv, env = visit_bv_term env bv in
      mk_bv_unop u bv, env

    | BvBnop (b,bv1,bv2) ->
      let bv1, env = visit_bv_term env bv1 in
      let bv2, env = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2, env
    | BvIte (bl,bv1,bv2) ->
      let bl, env = visit_bl_term env bl in
      let bv1, env = visit_bv_term env bv1 in
      let bv2, env = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2, env
    | Select (n,ax,bv) ->
      let bv, env = visit_bv_term env bv in
      let ax, env = visit_ax_term env ax in
      resolve_select env n ax bv

  and visit_ax_term env ax: ax_term*env =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) -> mk_ax_fun v ls, env

    | AxLet (bn,ax) ->
      assert (no_ax_def bn);
      let ax, base = visit_ax_term env ax in
      mk_ax_let bn ax, base

    | AxIte (bl,ax1,ax2) ->
      let ax1, env = visit_ax_term env ax1 in
      let ax2, env = visit_ax_term env ax2 in
      let bl, env = visit_bl_term env bl in
      mk_ax_ite bl ax1 ax2, env
    | Store (n,ax,bv1,bv2) ->
      let ax, env = visit_ax_term env ax in
      let bv1, env = visit_bv_term env bv1 in
      let bv2, env = visit_bv_term env bv2 in
      mk_store n ax bv1 bv2, env

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      assert (no_ax_decl ls);
      let bl, env = visit_bl_term env bl in
      Some(mk_bl_def v ls bl), env
    | BvDef (v,ls,bv) ->
      assert (no_ax_decl ls);
      let bv, env = visit_bv_term env bv in
      Some(mk_bv_def v ls bv), env
    | AxDef (v,ls,ax) ->
      assert (no_ax_decl ls);
      let ax, env = visit_ax_term env ax in
      let env = { env with bindmap = BindMap.ax_store env.bindmap v ax; } in
      None, env

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      let res = match dc.decl_desc with
        | AxDecl _ -> None
        | _ -> Some(mk_declare dc)
      in
      res, env
    | Define df ->
      let def, env = visit_def env df in
      (match def with None -> None | Some(x) -> Some(mk_define x)), env
    | Assert bl ->
      let bl, env = visit_bl_term env bl in
      Some(mk_assert bl), env
    | Assume bl ->
      let bl, env = visit_bl_term env bl in
      Some(mk_assume bl), env
    | Comment s -> Some(mk_comment s), env
    | Custom s -> Some(entry (Custom(s))), env
end

(** removes all arrays. complexity is terrible. *)
let remove_arrays fm =
  let fm, env = fold_forward
      (fun entry (fm, env) ->
         let entry, env = RemoveArrays.visit_entry env entry in
         let fm = match entry with
           | None -> fm
           | Some(e) -> push_front e fm
         in
         (fm, env))
      fm (empty, RemoveArrays.empty)
  in
  (* declare newly introduced bv *)
  let fm = AxVarMap.fold (fun _ l fm ->
      List.fold_left (fun fm (_, var) ->
          let decl = mk_declare (mk_bv_decl var []) in
          push_back decl fm
        ) fm l
    ) env.RemoveArrays.base_arrays fm
  in
  (*might break*)
  (*check_bindings fm*)
  fm

