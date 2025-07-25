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

module Inputs =
    struct
        module Input =
            struct
                type t =
                    {
                        name : string;
                        desc : string list;
                        id : Identifier.t
                    }

                let create cnt name desc =
                    {name; desc; id = Identifier.create ~cnt name}

                let get_name f =
                    f.name

                let get_id f =
                    f.id

                let get_desc f =
                    f.desc

                let pp f =
                    (Identifier.pp_basic f.id) ^ "_input"

                let compare f1 f2 =
                    Identifier.compare f1.id f2.id

                let equals f1 f2 =
                    Identifier.equals f1.id f2.id
            end

        module InputMap = Map.Make (Input)

        type 'a t =
            {
                inputs : 'a InputMap.t;
                cnt : int
            }

        let empty =
            {inputs = InputMap.empty; cnt = 0}

        let add name desc a inputs =
            let cnt = ref inputs.cnt in
            let input = Input.create cnt name desc in
            let cnt = !cnt in
            let inputs = InputMap.add input a inputs.inputs in
            input, {inputs; cnt}

        let find input inputs =
            InputMap.find input inputs.inputs

        let iter f inputs =
            InputMap.iter f inputs.inputs

        let fold f inputs acc =
            InputMap.fold f inputs.inputs acc

        let attribute_vars vars inputs =
            InputMap.fold
                (
                    fun k _ inputs ->
                        let prefix = Input.pp k in
                        try
                            let idx, var = List.find
                                (
                                    fun (_, var) ->
                                        match var with
                                        | Binsec.Formula.BvVar(var) -> String.starts_with ~prefix var.Binsec.Formula.bv_name 
                                        | _ -> false
                                )
                                @@ List.mapi (fun idx var -> idx, var) vars
                            in
                            {inputs with inputs = InputMap.add k (idx, var) inputs.inputs}
                        with Not_found -> inputs
                )
                inputs.inputs {empty with cnt = inputs.cnt}
    end

module Projections =
    struct
        module Projection =
            struct
                type t =
                    {
                        name : string;
                        suff : string;
                        id : Identifier.t
                    }

                let create cnt name suff =
                    {name; suff; id = Identifier.create ~cnt (name ^ "_" ^ suff)}

                let get_id p =
                    p.id

                let get_name p =
                    p.name

                let get_suffix p =
                    p.suff

                let pp p =
                    Identifier.pp_basic p.id

                let compare a b =
                    Identifier.compare a.id b.id

                let equals a b =
                    Identifier.equals a.id b.id
            end

        module ProjMap = Map.Make (Projection)

        type 'a t =
            {
                projs : 'a ProjMap.t;
                cnt : int
            }

        let empty = {projs = ProjMap.empty; cnt = 0}

        let add ?(suff = "proj") name a projs =
            let cnt = ref projs.cnt in
            let proj = Projection.create cnt name suff in
            let cnt = !cnt in
            let projs = ProjMap.add proj a projs.projs in
            proj, {projs; cnt}

        let find proj projs =
            ProjMap.find proj projs.projs

        let find_first f projs =
            ProjMap.find_first f projs.projs

        let remove proj projs =
            let cnt = projs.cnt in
            let projs = ProjMap.remove proj projs.projs in
            {projs; cnt}

        let iter f projs =
            ProjMap.iter f projs.projs

        let fold f projs acc =
            ProjMap.fold f projs.projs acc

        let update f projs =
            let cnt = projs.cnt in
            let projs = f projs.projs in
            {cnt; projs}

        let find_in ?(suff = "proj") fml =
            let projexp = Str.regexp ({|^\(.*\)_|} ^ (Str.quote suff) ^ {|_\([0-9]+\)$|}) in
            let find_proj entry projs =
                match entry.Binsec.Formula.entry_desc with
                | Binsec.Formula.Define({Binsec.Formula.def_desc = Binsec.Formula.BvDef(bv, _, _); _}) ->
                (
                    if Str.string_match projexp bv.Binsec.Formula.bv_name 0
                    then
                    (
                        let name = Str.matched_group 1 bv.bv_name in
                        let num = int_of_string @@ Str.matched_group 2 bv.bv_name in
                        let proj = Projection.create (ref num) name suff in
                        let cnt = max num projs.cnt in
                        let projs = ProjMap.add proj true projs.projs in
                        {projs; cnt}
                    )
                    else projs
                )
                | _ -> projs
            in
            Binsec.Formula.fold_forward find_proj fml empty
    end

module StatusVal =
    struct
        type t = Sat of (Binsec.Formula.bv_term * Z.t) list | Unsat | Error

        let to_generic = function
            | Sat(_) -> Result.Str("sat")
            | Unsat -> Result.Str("unsat")
            | Error -> Result.Str("error")
    end

module SolverStats =
    struct
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

        let create ?(report = true) name =
            let z3_cnt = new Stats.counter "z3" "Z3 queries" 0 in
            let bitw_cnt = new Stats.counter "bitw" "Bitwuzla queries" 0 in
            let q3b_cnt = new Stats.counter "q3b" "Q3B queries" 0 in
            (*let q3b_pbdd_cnt = new Stats.counter "q3b_pbdd" "Q3B_pBDD queries" 0 in*)
            let sat_cnt = new Stats.counter "sat cnt" "Sat" 0 in
            let unsat_cnt = new Stats.counter "unsat cnt" "Unsat" 0 in
            let err_cnt = new Stats.counter "err cnt" "Error" 0 in
            let timer = new Stats.timer "timer" "Solver runtime" in
            let b =
                Stats.Bundle.create ~report name
                    |> Stats.Bundle.add_stat (z3_cnt :> Stats.stat)
                    |> Stats.Bundle.add_stat (bitw_cnt :> Stats.stat)
                    |> Stats.Bundle.add_stat (q3b_cnt :> Stats.stat)
                    (*|> Stats.Bundle.add_stat (q3b_pbdd_cnt :> Stats.stat)*)
                    |> Stats.Bundle.add_stat (sat_cnt :> Stats.stat)
                    |> Stats.Bundle.add_stat (unsat_cnt :> Stats.stat)
                    |> Stats.Bundle.add_stat (err_cnt :> Stats.stat)
                    |> Stats.Bundle.add_stat (timer :> Stats.stat)
            in
            {z3_cnt; bitw_cnt; q3b_cnt; sat_cnt; unsat_cnt; err_cnt; timer; b}

        let incrres res ss =
            match res with
            | StatusVal.Sat(_) -> ss.sat_cnt#incr
            | Unsat -> ss.unsat_cnt#incr
            | Error -> ss.err_cnt#incr

        let incrsolv solver ss =
            match solver with
            | "z3" -> ss.z3_cnt#incr
            | "q3b" -> ss.q3b_cnt#incr
            | "bitwuzla" -> ss.bitw_cnt#incr
            | s when String.starts_with ~prefix:"old bitwuzla" s -> ss.bitw_cnt#incr
            | _ -> ()

        let ss = create "Property Solver"

        let registered = ref false

        let register () =
            if not !registered
            then
            (
                Stats.register ~big:false ss.b;
                registered := true
            )
    end

module HashMap = Utils.IntMap

type constr_builder = Binsec.Formula.var -> Binsec.Formula.term list -> Binsec.Formula.bl_term

module FormulaUtils =
    struct
        let clone_cnt = ref 0

        let clone_var ?(name = "") v =
            let mk_vname vname =
                let name = if name = "" then (vname ^ "_clone") else name in
                let name = Printf.sprintf "%s_%d" name !clone_cnt in
                clone_cnt := !clone_cnt + 1;
                name
            in
            match v with
            | Binsec.Formula.BlVar(v) -> Binsec.Formula.BlVar(Binsec.Formula.bl_var @@ mk_vname v.Binsec.Formula.bl_name)
            | Binsec.Formula.BvVar(v) -> BvVar(Binsec.Formula.bv_var (mk_vname v.Binsec.Formula.bv_name) v.Binsec.Formula.bv_size)
            | Binsec.Formula.AxVar(v) -> AxVar(Binsec.Formula.ax_var (mk_vname v.Binsec.Formula.ax_name) v.Binsec.Formula.idx_size v.Binsec.Formula.elt_size)

        let clone_decl ?(name = "") decl =
            Binsec.Formula.decl @@
                match decl.Binsec.Formula.decl_desc with
                | Binsec.Formula.BlDecl(bl, args) ->
                (
                    match clone_var ~name (BlVar(bl)) with
                    | BlVar(v) -> Binsec.Formula.BlDecl(v, args)
                    | _ -> assert false
                )
                | Binsec.Formula.BvDecl(bv, args) ->
                (
                    match clone_var ~name (BvVar(bv)) with
                    | BvVar(v) -> Binsec.Formula.BvDecl(v, args)
                    | _ -> assert false
                )
                | Binsec.Formula.AxDecl(ax, args) ->
                (
                    match clone_var ~name (AxVar(ax)) with
                    | AxVar(v) -> Binsec.Formula.AxDecl(v, args)
                    | _ -> assert false
                )

        let interval i bv =
            let lo = 
                let lo = Binsec.Formula.mk_bv_cst
                    @@ Binsec.Bitvector.create i.Interval.lo bv.Binsec.Formula.bv_term_size
                in
                Binsec.Formula.mk_bv_comp BvUle lo bv 
            in
            let hi = 
                let hi = Binsec.Formula.mk_bv_cst
                    @@ Binsec.Bitvector.create (Z.sub i.Interval.hi Z.one) bv.Binsec.Formula.bv_term_size
                in
                Binsec.Formula.mk_bv_comp BvUge hi bv
            in
            Binsec.Formula.mk_bl_and lo hi

        let var_to_decl var args =
            match var with
            | Binsec.Formula.BlVar(bl) -> Binsec.Formula.mk_bl_decl bl args
            | BvVar(bv) -> Binsec.Formula.mk_bv_decl bv args
            | AxVar(ax) -> Binsec.Formula.mk_ax_decl ax args

        let def_var_args def =
            match def.Binsec.Formula.def_desc with
            | Binsec.Formula.BlDef(bl, args, _) -> Binsec.Formula.BlVar(bl), args
            | Binsec.Formula.BvDef(bv, args, _) -> Binsec.Formula.BvVar(bv), args
            | Binsec.Formula.AxDef(ax, args, _) -> Binsec.Formula.AxVar(ax), args

        let decl_to_term decl args =
            match decl.Binsec.Formula.decl_desc with
            | Binsec.Formula.BlDecl(bl, args_decl) -> 
            (
                assert (List.length args = List.length args_decl);
                Binsec.Formula.mk_bl_term @@ Binsec.Formula.bl_term (BlFun(bl, args))
            )
            | Binsec.Formula.BvDecl(bv, args_decl) -> 
            (
                assert (List.length args = List.length args_decl);
                Binsec.Formula.mk_bv_term @@ Binsec.Formula.bv_term (BvFun(bv, args))
            )
            | Binsec.Formula.AxDecl(ax, args_decl) -> 
            (
                assert (List.length args = List.length args_decl);
                Binsec.Formula.mk_ax_term @@ Binsec.Formula.ax_term (AxFun(ax, args))
            )

        let def_to_term def args =
            match def.Binsec.Formula.def_desc with
            | Binsec.Formula.BlDef(bl, args_def, _) -> 
            (
                assert (List.length args = List.length args_def);
                Binsec.Formula.mk_bl_term @@ Binsec.Formula.bl_term (BlFun(bl, args))
            )
            | Binsec.Formula.BvDef(bv, args_def, _) -> 
            (
                assert (List.length args = List.length args_def);
                Binsec.Formula.mk_bv_term @@ Binsec.Formula.bv_term (BvFun(bv, args))
            )
            | Binsec.Formula.AxDef(ax, args_def, _) -> 
            (
                assert (List.length args = List.length args_def);
                Binsec.Formula.mk_ax_term @@ Binsec.Formula.ax_term (AxFun(ax, args))
            )

        let get_bl t =
            match t.Binsec.Formula.term_desc with
            | Binsec.Formula.BlTerm(t) -> t
            | _ -> assert false

        let get_bv t =
            match t.Binsec.Formula.term_desc with
            | Binsec.Formula.BvTerm(t) -> t
            | _ -> assert false

        let get_ax t =
            match t.Binsec.Formula.term_desc with
            | Binsec.Formula.AxTerm(t) -> t
            | _ -> assert false

        let app_bl f t =
            get_bl @@ f @@ Binsec.Formula.mk_bl_term t

        let app_bv f t =
            get_bv @@ f @@ Binsec.Formula.mk_bv_term t

        let app_ax f t =
            get_ax @@ f @@ Binsec.Formula.mk_ax_term t

        module Quant =
            struct

                type quant = Exists | Forall | NotExists | NotForall
        
                type quantdesc = quant * Binsec.Formula.decl list
        
                type quantbody = Body of Binsec.Formula.bl_term | NestedQuant of quantdesc * quantbody

                type quantentry = quantdesc * quantbody
        
                type Binsec.Formula.custom_desc += Quant of quantentry
        
                let quant_hash (qd, b) =
                    let qdh qd =
                        let q, decls = qd in
                        let qh = match q with
                            | Exists -> 0
                            | Forall -> 1
                            | NotExists -> 2
                            | NotForall -> 3
                        in
                        let declh = List.map (fun decl -> decl.Binsec.Formula.decl_hash) decls in
                        qh::declh
                    in
                    let rec aux res = function
                        | Body(bl) -> bl.Binsec.Formula.bl_term_hash::res
                        | NestedQuant(desc, b) -> aux ((qdh desc) @ res) b
                    in
                    Hashtbl.hash @@ aux (qdh qd) b
        
                let quant_to_smtlib q =
                    let not cons =
                        fun decls b -> Binsec.Smtlib_utils.mk_term_qual_identifier_terms (Binsec.Smtlib_utils.mk_qual_identifier_identifier @@ Binsec.Smtlib_utils.mk_id_symbol @@ Binsec.Smtlib_utils.mk_symbol "not") [cons decls b]
                    in
                    let rec cons = function
                        | Exists -> Binsec.Smtlib_utils.mk_term_exists_term
                        | Forall -> Binsec.Smtlib_utils.mk_term_forall_term
                        | NotExists -> not @@ cons Exists
                        | NotForall -> not @@ cons Forall
                    in
                    let mk_bitvec_symbol size = 
                        Binsec.Smtlib_utils.mk_symbol @@ Printf.sprintf "(_ BitVec %i)" size
                    in
                    let mk_array_symbol idx elt = 
                        Binsec.Smtlib_utils.mk_symbol @@ Printf.sprintf "(Array (_ BitVec %i) (_ BitVec %i))" idx elt
                    in
                    let of_sort = function
                        | Binsec.Formula.BlSort -> Binsec.Smtlib_utils.mk_sort_identifier @@ Binsec.Smtlib_utils.mk_symbol "Bool"
                        | Binsec.Formula.BvSort(i) -> Binsec.Smtlib_utils.mk_sort_identifier @@ mk_bitvec_symbol i
                        | Binsec.Formula.AxSort(i, j) -> Binsec.Smtlib_utils.mk_sort_identifier @@ mk_array_symbol i j
                    in
                    let of_decl = function
                        | Binsec.Formula.BlDecl(v, ls) ->
                        ( 
                            Binsec.Smtlib_utils.mk_symbol v.Binsec.Formula.bl_name,
                            Binsec.Smtlib_utils.mk_sort_identifier @@ Binsec.Smtlib_utils.mk_symbol "Bool",
                            List.map of_sort ls 
                        )
                        | Binsec.Formula.BvDecl(v, ls) ->
                        ( 
                            Binsec.Smtlib_utils.mk_symbol v.Binsec.Formula.bv_name,
                            Binsec.Smtlib_utils.mk_sort_identifier @@ mk_bitvec_symbol v.Binsec.Formula.bv_size,
                            List.map of_sort ls 
                        )
                        | Binsec.Formula.AxDecl(v, ls) ->
                        ( 
                            Binsec.Smtlib_utils.mk_symbol v.Binsec.Formula.ax_name,
                            Binsec.Smtlib_utils.mk_sort_identifier @@ mk_array_symbol v.Binsec.Formula.idx_size v.Binsec.Formula.elt_size,
                            List.map of_sort ls 
                        )
                    in
                    let of_decls decls = 
                        List.map 
                            (
                                fun decl -> 
                                    let symbol, sort, sorts = of_decl decl.Binsec.Formula.decl_desc in
                                    assert (sorts = []);
                                    Binsec.Smtlib_utils.mk_sorted_var symbol sort
                            )
                            decls
                    in
                    let rec aux ((q, decls), b) =
                        match b with
                        | Body(bl) -> (cons q) (of_decls decls) @@ Binsec.Formula_to_smtlib.bl_term bl
                        | NestedQuant(nq, nb) -> (cons q) (of_decls decls) @@ aux (nq, nb)
                    in
                    aux q
        
                let pp_quant fmt q =
                    Format.fprintf fmt "@[assert %a@]" Binsec.Smtlib_pp.pp_term @@ quant_to_smtlib q

                let mk_quant quant decls b =
                    (quant, decls), b
        
                let mk_quant_exists decls bl =
                    (Exists, decls), Body(bl)
        
                let mk_quant_forall decls bl =
                    (Forall, decls), Body(bl)

                let mk_quant_notexists decls bl =
                    (NotExists, decls), Body(bl)

                let mk_quant_notforall decls bl =
                    (NotForall, decls), Body(bl)

                let to_entry q =
                    Binsec.Formula.entry (Binsec.Formula.Custom(Quant(q)))
        
                let _ =
                    let hash = function
                        | Quant(q, b) -> Some(quant_hash (q, b))
                        | _ -> None
                    in
                    let pp fmt = function
                        | Quant(q, b) -> 
                        (
                            pp_quant fmt (q, b);
                            true
                        )
                        | _ -> false
                    in
                    Binsec.Formula.register_custom {Binsec.Formula.hash; Binsec.Formula.pp}
            end

        module CustomCmd =
            struct
                module type Sig =
                    sig
                        type Binsec.Formula.custom_desc += Cmd of Binsec.Formula.term

                        val mk_cmd : Binsec.Formula.term -> Binsec.Formula.entry
                    end

                let make cmd =
                    let module M =
                        struct
                            type Binsec.Formula.custom_desc += Cmd of Binsec.Formula.term

                            let hash t =
                                Hashtbl.hash (Hashtbl.hash cmd, t.Binsec.Formula.term_hash)

                            let pp fmt t =
                                Format.fprintf fmt "@[%s %a@]" cmd Binsec.Smtlib_pp.pp_term @@ Binsec.Formula_to_smtlib.term t

                            let mk_cmd t =
                                Binsec.Formula.entry (Binsec.Formula.Custom(Cmd(t)))

                            let _ =
                                let hash = function
                                    | Cmd(t) -> Some(hash t)
                                    | _ -> None
                                in
                                let pp fmt = function
                                    | Cmd(t) ->
                                    (
                                        pp fmt t;
                                        true
                                    )
                                    | _ -> false
                                in
                                Binsec.Formula.register_custom {Binsec.Formula.hash; Binsec.Formula.pp}
                        end
                    in
                    (module M : Sig)

                module Maximize = (val (make "maximize"))
                module Minimize = (val (make "minimize"))
            end
    end

module Status = Result.Make(StatusVal)

module MultiSolver =
    struct
        type sat = Cadical | CMS | Lingeling | Minisat | Picosat | Kissat | Gimsatul
        type smt = Z3 | OldBitwuzla of sat | Q3B | Q3B_pBDD | Bitwuzla

        let sat_str = function
            | Cadical -> "cadical"
            | CMS -> "cms"
            | Lingeling -> "lingeling"
            | Minisat -> "minisat"
            | Picosat -> "picosat"
            | Kissat -> "kissat"
            | Gimsatul -> "gimsatul"

        let smt_str = function
            | Z3 -> "z3"
            | OldBitwuzla(sat) -> Printf.sprintf "old bitwuzla (%s)" @@ sat_str sat
            | Q3B -> "q3b"
            | Q3B_pBDD -> "q3b_pbdd"
            | Bitwuzla -> "bitwuzla"

        let smt_binsec = function
            | Z3 -> Smt.Smt_options.Z3_smtlib
            | Q3B_pBDD
            | Q3B -> assert false
            | OldBitwuzla(_)
            | Bitwuzla -> Smt.Smt_options.Bitwuzla_smtlib

        let all_solvers =
            [
                Z3;
                Bitwuzla;
                OldBitwuzla(Cadical);
                OldBitwuzla(CMS);
                OldBitwuzla(Lingeling);
                OldBitwuzla(Minisat);
                OldBitwuzla(Picosat);
                OldBitwuzla(Kissat);
                OldBitwuzla(Gimsatul);
                Q3B;
                (*Q3B_pBDD*)
            ]

        let compat = function
            | Some(solver), _, _, _ -> [solver]
            | None, true, _, _ -> [Z3; Bitwuzla; Q3B; Q3B_pBDD]
            | _ -> all_solvers

        let can_get solver quant get =
            match solver with
            | Q3B_pBDD
            | Q3B -> false
            | OldBitwuzla(_) -> not quant
            | _ -> true

        let opts = function
            | Z3 -> "--smt2"
            | OldBitwuzla(sat) -> "old -m -x -SE " ^ (sat_str sat)
            | Bitwuzla -> "-m --bv-output-format 16"
            | Q3B_pBDD
            | Q3B -> assert false

        let incr = function
            | Z3 -> SolverStats.ss.z3_cnt#incr
            | Bitwuzla
            | OldBitwuzla(_) -> SolverStats.ss.bitw_cnt#incr
            | Q3B_pBDD -> () (*SolverStats.ss.q3b_pbdd_cnt#incr*)
            | Q3B -> SolverStats.ss.q3b_cnt#incr

        let work solver fml quant get =
            let label = "check-sat" in
            match solver with
            | Q3B_pBDD
            | Q3B ->
            (
                Binsec.Formula_pp.pp_formula Format.str_formatter fml;
                let file = File.create (smt_str solver) "smt2" in
                File.write ((Format.flush_str_formatter ()) ^ "\n(check-sat)\n") file;
                let timeout = Binsec.Subprocess.spawn ~pdeathsig:Sys.sigkill [|"sleep"; Printf.sprintf "%d" @@ Binsec.Formula_options.Solver.Timeout.get ()|] in
                let proc = Binsec.Subprocess.spawn ~pdeathsig:Sys.sigkill [|smt_str solver; File.get_path file|] in
                let pid = Binsec.Subprocess.pid timeout in
                let pid2 = Binsec.Subprocess.pid proc in
                let pid3, _ = Unix.wait () in
                if pid3 = pid 
                then
                (
                    Unix.kill pid2 Sys.sigkill;
                    Status.create ~label (Error)
                )
                else
                (
                    Unix.kill pid Sys.sigkill;
                    (*let resexp = Str.regexp {|\(\(un\)?sat\)|} in*)
                    let res =
                        try
                            match input_line @@ Binsec.Subprocess.stdout proc with
                            | "sat" -> Status.create ~label (Sat([]))
                            | "unsat" -> Status.create ~label (Unsat)
                            | _ -> Status.create ~label (Error)
                        with _ -> Status.create ~label (Error)
                    in
                    res
                )
            )
            | _ ->
            (
                Smt.Smt_options.SMTSolver.set @@ smt_binsec solver;
                Binsec.Formula_options.Solver.Options.set @@ opts solver;
                let module Solver = (val Smt.Smt_solver.get_solver ()) in
                try
                    let session = Solver.open_session () in
                    Binsec.Formula.iter_forward (Solver.put session) fml;
                    match Solver.check_sat session with
                    | Binsec.Formula.SAT -> 
                    (
                        let get = 
                            if can_get solver quant get
                            then get
                            else []
                        in
                        let values = List.map (fun e -> (e, Binsec.Bitvector.value_of @@ Solver.get_bv_value session e)) get in
                        Solver.close_session session;
                        Status.create ~label (Sat(values))
                    )
                    | Binsec.Formula.UNSAT -> 
                    (
                        Solver.close_session session;
                        Status.create ~label (Unsat)
                    )
                    | _ -> 
                    (
                        Solver.close_session session;
                        Status.create ~label (Error)
                    )
                with e -> Status.create ~label (Error)
            )

        let check_sat solvers fml quant get =
            let label = "check-sat" in
            let timer = new Stats.timer "runtime" "Runtime" in
            let ssolver = new Stats.sstat "solver" "Solver" in
            ssolver#set "n/a";
            let stats = Stats.Bundle.create "" 
                |> Stats.Bundle.add_stat (ssolver :> Stats.stat)
                |> Stats.Bundle.add_stat (timer :> Stats.stat)
            in
            let stats = Some(stats) in
            let children = ref HashMap.empty in
            let aux solver =
                let pipr, pipw = Unix.pipe () in
                match Unix.fork () with
                | 0 ->
                (
                    Sys.set_signal Sys.sigint Sys.Signal_default;
                    Sys.set_signal Sys.sigsegv Sys.Signal_default;
                    Unix.close pipr;
                    let pipw_chan = Unix.out_channel_of_descr pipw in
                    let dead s =
                        Printf.fprintf pipw_chan "dead\n";
                        flush pipw_chan;
                        close_out pipw_chan;
                        exit 0
                    in
                    Sys.set_signal Sys.sigpipe (Sys.Signal_handle(dead));
                    (
                        let res = work solver fml quant get in
                        match res.Result.result with
                        | StatusVal.Sat(values) ->
                        (
                            Printf.fprintf pipw_chan "sat\n";
                            List.iter (fun (_, z) -> Printf.fprintf pipw_chan "%s\n" @@ Z.to_string z) values;
                        )
                        | Unsat -> Printf.fprintf pipw_chan "unsat\n"
                        | Error -> Printf.fprintf pipw_chan "error\n"
                    )
                    ;
                    flush pipw_chan;
                    close_out pipw_chan;
                    exit 0
                )
                | pid ->
                (
                    Unix.close pipw;
                    children := HashMap.add pid (solver, (Unix.in_channel_of_descr pipr)) !children
                )
                | exception e ->
                (
                    Unix.close pipr;
                    Unix.close pipw;
                    raise e
                )
            in
            timer#start;
            SolverStats.ss.timer#start;
            List.iter aux solvers;
            (*in case a solver ignores timeout*)
            let safety =
                match Unix.fork () with
                | 0 ->
                (
                    Sys.set_signal Sys.sigint Sys.Signal_default;
                    Sys.set_signal Sys.sigsegv Sys.Signal_default;
                    Sys.set_signal Sys.sigpipe Sys.Signal_default;
                    while timer#get < (1.1 *. (Float.of_int @@ Binsec.Formula_options.Solver.Timeout.get ())) 
                    do
                        Unix.sleep 1
                    done;
                    exit 0
                )
                | pid -> pid
                | exception e -> raise e
            in
            let res = ref None in
            let tmp = ref None in
            while not (HashMap.is_empty !children)
            do
                let pid, status = Unix.wait () in
                match status with
                | Unix.WEXITED(0) ->
                (
                    if pid = safety
                    then
                    (
                        HashMap.iter (fun pid (_, chan) -> close_in chan; Unix.kill pid Sys.sigkill) !children;
                        children := HashMap.empty
                    )
                    else
                    (
                        try
                            let solver, chan = HashMap.find pid !children in
                            let lines = ref [] in
                            let lines =
                                try
                                    while true
                                    do
                                        lines := (input_line chan)::!lines
                                    done;
                                    List.rev !lines
                                with End_of_file -> List.rev !lines
                            in
                            res := Some
                                (
                                    match lines with
                                    | "sat"::l ->
                                    (
                                        try
                                            let values = List.map2 (fun e z -> e, Z.of_string z) get l in
                                            timer#stop;
                                            SolverStats.ss.timer#stop;
                                            SolverStats.ss.sat_cnt#incr;
                                            incr solver;
                                            ssolver#set @@ smt_str solver;
                                            Status.create ~label ~stats (Sat(values))
                                        with _ ->
                                        (
                                            ssolver#set @@ smt_str solver;
                                            tmp := Some(solver, Status.create ~label ~stats (Sat([])));
                                            close_in chan;
                                            raise (Failure "")
                                        )
                                    )
                                    | "unsat"::_ -> 
                                    (
                                        timer#stop;
                                        SolverStats.ss.timer#stop;
                                        SolverStats.ss.unsat_cnt#incr;
                                        incr solver;
                                        ssolver#set @@ smt_str solver;
                                        Status.create ~label ~stats (Unsat)
                                    )
                                    | msg::_ ->
                                    (
                                        Message.Wrap.send (Message.Wrap.Debug("SMT", lazy ((smt_str solver) ^ ": " ^ msg)));
                                        close_in chan;
                                        raise (Failure "")
                                    )
                                    | _ -> 
                                    (
                                        close_in chan;
                                        raise (Failure "")
                                    )
                                )
                            ;
                            close_in chan;
                            children := HashMap.remove pid !children;
                            HashMap.iter (fun pid (_, chan) -> close_in chan; Unix.kill pid Sys.sigkill) !children;
                            children := HashMap.empty
                        with _ -> children := HashMap.remove pid !children
                    )
                )
                | _ -> children := HashMap.remove pid !children
            done;
            timer#stop;
            SolverStats.ss.timer#stop;
            match !res, !tmp with
            | Some(res), _ -> res
            | None, Some(solver, tmp) -> 
            (
                incr solver;
                SolverStats.ss.sat_cnt#incr;
                ssolver#set @@ smt_str solver;
                tmp
            )
            | None, None -> 
            (
                SolverStats.ss.err_cnt#incr;
                Status.create ~label ~stats (Error)
            )
    end

open FormulaUtils

type t =
    {
        fml : Binsec.Formula.formula;

        free : Binsec.Formula.var list;
        inputs : (int * Binsec.Formula.var) Inputs.t;
        decls : Binsec.Formula.decl list;
        projs : Binsec.Formula.def Projections.t;
        defs : Binsec.Formula.def list;
        constr : Binsec.Formula.def;

        custom_decls : Binsec.Formula.decl list;
        custom_defs : Binsec.Formula.def list;
        custom_asserts : Binsec.Formula.bl_term list;
        custom : Binsec.Formula.entry list;
        proj_constrs : constr_builder Projections.ProjMap.t;
        proj_constrs_def : Binsec.Formula.def;
        
        has_quant : bool;
        has_free_array : bool;
        solver : MultiSolver.smt option
    }

let create ?(inputs = Inputs.empty) ?(projections = Projections.empty) fml =
    let def_is_proj def proj =
        match def.Binsec.Formula.def_desc with
        | Binsec.Formula.BvDef(var, _, _) -> var.bv_name = Projections.Projection.pp proj
        | _ -> false
    in
    let is_proj def =
        try
            Some(Projections.find_first (def_is_proj def) projections)
        with Not_found -> None
    in
    let fml =
        if projections = Projections.empty
        then fml
        else
        (
            let get_projs e projs =
                match e.Binsec.Formula.entry_desc with
                | Define(def) -> 
                (
                    match is_proj def with
                    | Some(_) ->
                    (
                        match def.def_desc with
                        | BvDef(var, _, _) -> Binsec.Formula.VarSet.add (Binsec.Formula.BvVar(var)) projs
                        | _ -> projs
                    )
                    | _ -> projs
                )
                | _ -> projs
            in
            let keep = Binsec.Formula.fold_backward get_projs fml Binsec.Formula.VarSet.empty in
            Binsec.Formula_transformation.optimize_from_options ?is_controlled:None ~keep fml
        )
    in
    let module Entries =
        struct
            type t =
                {
                    free : Binsec.Formula.var list;
                    free_decls : Binsec.Formula.decl list;
                    free_terms : Binsec.Formula.term list;
                    decls : Binsec.Formula.decl list;
                    defs : Binsec.Formula.def list;
                    constrs : Binsec.Formula.bl_term list
                }

            let empty =
                {
                    free = [];
                    free_decls = [];
                    free_terms = [];
                    decls = [];
                    defs = [];
                    constrs = []                
                }
        end
    in
    let free_arr = ref false in
    let sort_entry e entries =
        match e.Binsec.Formula.entry_desc with
        | Declare(decl) -> 
        (
            let var, nargs =
                match decl.Binsec.Formula.decl_desc with
                | BvDecl(var, args) -> Binsec.Formula.BvVar(var), args = []
                | BlDecl(var, args) -> Binsec.Formula.BlVar(var), args = []
                | AxDecl(var, []) -> 
                (
                    free_arr := true;
                    Binsec.Formula.AxVar(var), true
                )
                | AxDecl(var, _) -> Binsec.Formula.AxVar(var), false
            in
            if nargs
            then
            (
                let free = var::entries.Entries.free in
                let free_decls = decl::entries.free_decls in
                let free_terms = (decl_to_term decl [])::entries.free_terms in
                {entries with free; free_decls; free_terms}
            )
            else
            (
                let decls = decl::entries.decls in
                {entries with decls}
            )
        )
        | Define(def) ->
        (
            let defs = def::entries.defs in
            {entries with defs}
        )
        | Assert(c) ->
        (
            let constrs = c::entries.constrs in
            {entries with constrs}
        )
        | _ -> entries
    in
    let entries = Binsec.Formula.fold_backward sort_entry fml Entries.empty in
    let inputs = Inputs.attribute_vars entries.free inputs in
    let rec parameterize_term e =
        let get_bl = app_bl parameterize_term in
        let get_bv = app_bv parameterize_term in
        let get_ax = app_ax parameterize_term in
        let is_defined var =
            let free = List.find_opt
                (
                    fun e -> 
                        match e, var with
                        | (Binsec.Formula.BlVar(v)), (Binsec.Formula.BlVar(w)) -> v = w
                        | (Binsec.Formula.BvVar(v)), (Binsec.Formula.BvVar(w)) -> v = w
                        | (Binsec.Formula.AxVar(v)), (Binsec.Formula.AxVar(w)) -> v = w
                        | _, _ -> false
                )
                entries.free
            in
            let decl = List.find_opt
                (
                    fun e ->
                        match e.Binsec.Formula.decl_desc, var with
                        | (Binsec.Formula.BlDecl(v, _)), (Binsec.Formula.BlVar(w)) -> v = w
                        | (Binsec.Formula.BvDecl(v, _)), (Binsec.Formula.BvVar(w)) -> v = w
                        | (Binsec.Formula.AxDecl(v, _)), (Binsec.Formula.AxVar(w)) -> v = w
                        | _, _ -> false
                )
                entries.decls
            in
            match free, decl with
            | None, None -> true
            | _, _ -> false
        in
        match e.term_desc with
        | Binsec.Formula.BlTerm(t) ->         
        (
            let desc =
                match t.Binsec.Formula.bl_term_desc with
                | BlTrue -> Binsec.Formula.BlTrue
                | BlFalse -> Binsec.Formula.BlFalse
                | BlFun(f, args) -> 
                (
                    if is_defined (BlVar(f))
                    then
                    (
                        let args = List.map parameterize_term args in
                        BlFun(f, args @ entries.free_terms)
                    )
                    else BlFun(f, args)
                )
                | BlLet(defs, t) -> BlLet(defs, get_bl t)
                | BlUnop(op, t) -> BlUnop(op, get_bl t)
                | BlBnop(op, t1, t2) -> BlBnop(op, get_bl t1, get_bl t2)
                | BlComp(cmp, t1, t2) -> BlComp(cmp, get_bl t1, get_bl t2)
                | BvComp(cmp, t1, t2) -> BvComp(cmp, get_bv t1, get_bv t2)
                | AxComp(cmp, t1, t2) -> AxComp(cmp, get_ax t1, get_ax t2)
                | BlIte(t1, t2, t3) -> BlIte(get_bl t1, get_bl t2, get_bl t3)
            in
            Binsec.Formula.mk_bl_term @@ Binsec.Formula.bl_term desc
        )
        | BvTerm(t) ->
        (
            let desc =
                match t.Binsec.Formula.bv_term_desc with
                | BvCst(bv) -> Binsec.Formula.BvCst(bv)
                | BvFun(f, args) ->
                (
                    if is_defined (BvVar(f))
                    then
                    (
                        let args = List.map parameterize_term args in
                        BvFun(f, args @ entries.free_terms)
                    )
                    else BvFun(f, args)
                )
                | BvLet(defs, t) -> BvLet(defs, get_bv t)
                | BvUnop(op, t) -> BvUnop(op, get_bv t)
                | BvBnop(op, t1, t2) -> BvBnop(op, get_bv t1, get_bv t2)
                | BvIte(t1, t2, t3) -> BvIte(get_bl t1, get_bv t2, get_bv t3)
                | Select(i, t1, t2) -> Select(i, get_ax t1, get_bv t2)
            in
            Binsec.Formula.mk_bv_term @@ Binsec.Formula.bv_term desc
        )
        | AxTerm(t) ->
        (
            let desc = 
                match t.Binsec.Formula.ax_term_desc with
                | AxFun(f, args) -> 
                (
                    if is_defined (AxVar(f))
                    then
                    (
                        let args = List.map parameterize_term args in
                        Binsec.Formula.AxFun(f, args @ entries.free_terms)
                    )
                    else AxFun(f, args)
                )
                | AxLet(defs, t) -> AxLet(defs, get_ax t)
                | AxIte(t1, t2, t3) -> AxIte(get_bl t1, get_ax t2, get_ax t3)
                | Store(i, t1, t2, t3) -> Store(i, get_ax t1, get_bv t2, get_bv t3)
            in
            Binsec.Formula.mk_ax_term @@ Binsec.Formula.ax_term desc
        )
    in
    let parameterize e =
        match e.Binsec.Formula.def_desc with
        | BlDef(var, args, body) -> Binsec.Formula.def (BlDef(var, args @ entries.free_decls, app_bl parameterize_term body))
        | BvDef(var, args, body) -> Binsec.Formula.def (BvDef(var, args @ entries.free_decls, app_bv parameterize_term body))
        | AxDef(var, args, body) -> Binsec.Formula.def (AxDef(var, args @ entries.free_decls, app_ax parameterize_term body))
    in
    let defs = List.map parameterize entries.defs in
    let constr, proj_constrs_def =
        let constr = List.fold_left (fun res e -> Binsec.Formula.mk_bl_and res @@ app_bl parameterize_term e) Binsec.Formula.mk_bl_true entries.constrs in
        let proj_constrs_var = Binsec.Formula.bl_var "additional_constraints" in
        let constr = Binsec.Formula.mk_bl_and constr @@ Binsec.Formula.bl_term (Binsec.Formula.BlFun(proj_constrs_var, entries.free_terms)) in
        (Binsec.Formula.def (Binsec.Formula.BlDef(Binsec.Formula.bl_var "path_constraint", entries.free_decls, constr))),
        (Binsec.Formula.def (Binsec.Formula.BlDef(proj_constrs_var, entries.free_decls, Binsec.Formula.mk_bl_true)))
    in
    let projs = Projections.update
        (
            Projections.ProjMap.filter_map
                (
                    fun proj _ -> List.find_opt (fun def -> def_is_proj def proj) defs
                )
        )
        projections
    in
    let custom_decls, custom_defs, custom_asserts, custom = [], [], [], [] in
    let proj_constrs = Projections.ProjMap.empty in
    {fml; free = entries.free; inputs; decls = entries.decls; projs; defs; constr; custom_decls; custom_defs; custom_asserts; custom; proj_constrs; proj_constrs_def; has_quant = false; has_free_array = !free_arr; solver = None}

let create ?(inputs = Inputs.empty) ?(projections = Projections.empty) fml =
    SolverStats.register ();
    let sf = create ~inputs ~projections fml in
    if sf.has_free_array
    then 
    (
        let sf = create ~projections @@ ArrayBlasting.remove_arrays sf.fml in
        (
            match 
                List.find_opt
                (
                    fun var -> 
                        let name =
                            match var with
                            | Binsec.Formula.BvVar(var) -> var.Binsec.Formula.bv_name
                            | Binsec.Formula.BlVar(var) -> var.Binsec.Formula.bl_name
                            | Binsec.Formula.AxVar(var) -> var.Binsec.Formula.ax_name
                        in
                        String.starts_with ~prefix:"__array_symbolic_memory" name
                )
                sf.free
            with
            | Some(_) -> Message.Wrap.send (Message.Wrap.Warning(lazy "memory array elimination introduced symbolic memory bytes"))
            | _ -> ()
        )
        ;
        sf
    )
    else sf

let compile sf =
    let proj_constrs =
        let constr =
            Projections.fold
                (
                    fun proj def res ->
                    (
                        try
                            let constr_builder = Projections.ProjMap.find proj sf.proj_constrs in
                            let v, args = def_var_args def in
                            Binsec.Formula.mk_bl_and res
                                @@ constr_builder v
                                @@ List.map (fun e -> decl_to_term e []) args
                        with Not_found -> res
                    )
                )
                sf.projs Binsec.Formula.mk_bl_true
        in
        match sf.proj_constrs_def.def_desc with
        | BlDef(f, args, _) -> Binsec.Formula.def (BlDef(f, args, constr))
        | _ -> assert false
    in
    let switch f x y = f y x in
    Binsec.Formula.empty
        |> fun fml -> List.fold_left (switch Binsec.Formula.push_front_declare) fml sf.decls
        |> fun fml -> List.fold_left (switch Binsec.Formula.push_front_define) fml sf.defs
        |> Binsec.Formula.push_front_define proj_constrs
        |> Binsec.Formula.push_front_define sf.constr
        |> fun fml -> List.fold_left (switch Binsec.Formula.push_front_declare) fml sf.custom_decls
        |> fun fml -> List.fold_left (switch Binsec.Formula.push_front_define) fml sf.custom_defs
        |> fun fml -> List.fold_left (switch Binsec.Formula.push_front_assert) fml sf.custom_asserts
        |> fun fml -> List.fold_left (switch Binsec.Formula.push_front) fml sf.custom

let clean sf =
    let custom_decls, custom_defs, custom_asserts, custom = [], [], [], [] in
    let proj_constrs = Projections.ProjMap.empty in
    {sf with custom_decls; custom_defs; custom_asserts; proj_constrs; custom}

let pp sf =
    Binsec.Formula_pp.pp_formula Format.str_formatter @@ compile sf;
    Format.flush_str_formatter ()

let pp_base sf =
    Binsec.Formula_pp.pp_formula Format.str_formatter sf.fml;
    Format.flush_str_formatter ()

let get_base sf =
    sf.fml

let get_free_variables sf =
    sf.free

let get_inputs sf =
    sf.inputs

let get_projections sf =
    sf.projs

let get_declarations sf =
    sf.decls

let get_definitions sf =
    sf.defs

let get_path_constraint sf =
    sf.constr

let set_quant has_quant sf =
    {sf with has_quant}

let set_solver solver sf =
    {sf with solver}

let has_quant sf =
    sf.has_quant

let has_free_array sf =
    sf.has_free_array

let add_declare decl sf =
    let custom_decls = sf.custom_decls @ [decl] in
    {sf with custom_decls}

let add_define def sf =
    let custom_defs = sf.custom_defs @ [def] in
    {sf with custom_defs}

let add_assert a sf =
    let custom_asserts = sf.custom_asserts @ [a] in
    {sf with custom_asserts}

let add_custom c sf =
    let custom = sf.custom @ [c] in
    {sf with custom}

let add_proj_constr proj builder sf =
    let builder =
        try
            let old = Projections.ProjMap.find proj sf.proj_constrs in
            fun v args -> Binsec.Formula.mk_bl_and (old v args) (builder v args)
        with Not_found -> builder
    in
    let proj_constrs = Projections.ProjMap.add proj builder sf.proj_constrs in
    {sf with proj_constrs}

let get_proj_constr proj sf =
    Projections.ProjMap.find proj sf.proj_constrs

let remove_proj_constr proj sf =
    let proj_constrs = Projections.ProjMap.remove proj sf.proj_constrs in
    {sf with proj_constrs}

let add_proj_restrict proj ~lo ~hi sf =
    let name = Printf.sprintf "%s_bytes_%d_to_%d" (Projections.Projection.get_name proj) lo hi in
    let proj_def =
        try
            Projections.find proj @@ get_projections sf
        with Not_found -> assert false
    in
    let proj_var, proj_args = FormulaUtils.def_var_args proj_def in
    let proj_args_term = List.map (fun arg -> FormulaUtils.decl_to_term arg []) proj_args in
    let proj_term = FormulaUtils.def_to_term proj_def proj_args_term in
    let extract = Binsec.Formula.mk_bv_extract {Binsec.Interval.lo = lo * 8; Binsec.Interval.hi = hi * 8 + 7} @@ FormulaUtils.get_bv proj_term in
    let nvar = Binsec.Formula.bv_var name 8 in
    let ndef = Binsec.Formula.mk_bv_def nvar proj_args extract in
    let nproj, nprojs = Projections.add name ndef @@ get_projections sf in
    nproj, {sf with defs = sf.defs @ [ndef]; projs = nprojs}

let check_sat ?(get = []) sfml =
    let solvers = MultiSolver.compat (sfml.solver, sfml.has_free_array, sfml.has_quant, (not (get = []))) in
    let fml = compile sfml in
    MultiSolver.check_sat solvers fml sfml.has_quant get

let from_file file =
    let smt = Binsec.Parse_utils.read_file ~parser:Binsec.Smtlib_parser.script ~lexer:Binsec.Smtlib_lexer.token ~filename:(File.get_path file) in
    let fml = Binsec.Smtlib_to_formula.script smt in
    let projections = Projections.find_in fml in
    create ~projections fml
