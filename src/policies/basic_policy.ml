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

let dump_taint_opt = new Options.TaggedOptions.opt false
let check_overtaint_opt = new Options.TaggedOptions.opt false

module TT =
    struct
        type t = Controlled | Uncontrolled

        let from_source src =
            let aux = function
                | "controlled"
                | "c"
                | "C" -> Some(Controlled)
                | "uncontrolled"
                | "uc"
                | "UC"
                | "u"
                | "U" -> Some(Uncontrolled)
                | _ -> None
            in
            match List.find_map aux src.Source.desc with
            | Some(tt) -> [tt, src.Source.storage]
            | _ -> 
            (
                Message.Wrap.send (Message.Wrap.Warning(lazy(Printf.sprintf "no valid source description found in <%s>" @@ Utils.pp_list (fun e -> e) src.Source.desc)));
                []
            )

        let pp tt =
            match tt with
            | Some(Controlled) -> "C"
            | Some(Uncontrolled) -> "U"
            | None -> "-"

        let pp_list l =
            let aux res e =
                let sep = if res = "" then "" else ", " in
                res ^ sep ^ (pp e)
            in
            "<" ^
            (List.fold_left aux "" l) ^
            ">"

        let compare tt tt_ =
            let num tt =
                match tt with
                | Some(Uncontrolled) -> 2
                | Some(Controlled) -> 1
                | None -> 0
            in
            let x = num tt in
            let y = num tt_ in
            Some(x - y)

        let equals tt tt_ =
            match compare tt tt_ with
            | Some(0) -> true
            | _ -> false

        let to_generic tt =
            Result.Str(pp tt)
    end

module S =
    struct
        type t =
            {
                dump_taint : bool;
                check_overtaint : bool;
                symb : Symbolic_policy.SA.t option;
                nextsymb : Symbolic_policy.SA.t option
            }

        let create ?(tag = None) () =
            {dump_taint = false; check_overtaint = false; symb = None; nextsymb = None}

        let add_stats_to_bundle b _ = b

        let set_dump_taint dump_taint s =
            {s with dump_taint}

        let init_check_overtaint check_overtaint s =
            let symb =
                if check_overtaint
                then
                (
                    let symb = Symbolic_policy.init_analysis "Overtaint"
                        |> Symbolic_policy.SA.set_min_verbosity 4
                        |> Symbolic_policy.SA.set_ignore_sinks true
                    in
                    Some(symb)
                )
                else None
            in
            let nextsymb = symb in
            {s with check_overtaint; symb; nextsymb}
    end

module TB = Taint.MakeBank (TT)
module A = Analysis.Make (TB) (S)

let propag_callback instr a =
    let cmp tt tt_ =
        match TT.compare tt tt_ with
        | Some(res) -> res
        | None -> 0
    in
    let max = List.fold_left
        (
            fun res sto ->
                match TB.get_taint sto @@ A.get_bank a with
                | Some(l) -> List.fold_left (fun res e -> if (cmp e res) > 0 then e else res) res l
                | _ -> res
        )
        None instr.Instruction.reads 
    in
    let filter = function
        | Storage.Register(reg, _) -> not (reg = "rflags")
        | _ -> true
    in
    List.fold_left
        (
            fun a sto -> 
                if filter sto
                then A.set_bank a @@ TB.taint sto max @@ A.get_bank a
                else a
        )
        a
        instr.Instruction.writes

let dump_sink sink a =
    if (A.get_state a).dump_taint
    then Message.Wrap.send (Analysis.Wrap.SinkResult(sink, Result.Generic(Result.mk_string ~label:"taint bank" @@ TB.pp @@ A.get_bank a)))

let check_ot snk a =
    if (A.get_state a).check_overtaint
    then
    (
        match (A.get_state a).symb with
        | Some(symb) ->
        (
            let ot_reg = ref 0 in
            let ot_mem = ref 0 in
            let aux sto _ =
                match (Symbolic_policy.SB.get_mode @@ Symbolic_policy.SA.get_bank symb),
                    Symbolic_policy.SB.sink (Sink.create "" (Expr.Var(sto))) @@ Symbolic_policy.SA.get_bank symb 
                with
                | Symbolic, (_, false) ->
                (
                    match sto with
                    | Storage.Custom(_) -> ()
                    | Storage.Register(_, _) -> ot_reg := !ot_reg + 1
                    | Storage.Memory(_, _) -> ot_mem := !ot_mem + 1
                )
                | _, (_, false) -> raise (Failure("symbolic engine in concrete mode"))
                | _, (_, _) -> ()
            in
            try
                TB.iter_reg (fun reg tt -> aux (Storage.Register(reg, List.length tt)) tt) @@ A.get_bank a;
                TB.iter_mem (fun addr tt -> aux (Storage.Memory(addr, 1)) tt) @@ A.get_bank a;
                let snkot =
                    let _, positive = Symbolic_policy.SB.sink snk @@ Symbolic_policy.SA.get_bank symb in
                    not positive
                in
                let ressnk = Result.mk_bool ~label:"sink" snkot in
                let resreg = Result.mk_int ~label:"registers" @@ Z.of_int !ot_reg in
                let resmem = Result.mk_int ~label:"memory" @@ Z.of_int !ot_mem in
                Message.Wrap.send (Analysis.Wrap.SinkResult(snk, Result.Generic(Result.mk_res_list ~label:"Overtaint" [ressnk; resreg; resmem])))
            with Failure msg -> Message.Wrap.send (Analysis.Wrap.SinkResult(snk, Result.Generic(Result.mk_string ~label:"Overtaint" @@ Printf.sprintf "N/A (%s)" msg)))
        )
        | _ -> ()
    )
    else ()

let update_symb line a =
    let state = A.get_state a in
    match state.nextsymb with
    | Some(s) -> 
    (
        let symb = Some(s) in
        let nextsymb = Some(Symbolic_policy.SA.analyze line s) in
        A.set_state a {state with symb; nextsymb}
    )
    | None -> a
    
let init_analysis 
    ?(register_stats = true) 
    ?dump_taint 
    ?check_overtaint
    ?(tag = None)
        name =
    let dump_taint = dump_taint_opt#maybe_get ?tag dump_taint in
    let check_overtaint = check_overtaint_opt#maybe_get ?tag check_overtaint in
    let a = A.create ~tag name in
    let a = A.set_state a @@ S.set_dump_taint dump_taint @@ S.init_check_overtaint check_overtaint @@ A.get_state a in
    if register_stats
    then Stats.register ~silent:true @@ A.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ A.get_name a) a
    ;
    let todo a =
        a
            |> A.InstructionCallbacks.add "basic propagation" propag_callback
            |> A.SinkCallbacks.add_const "taint bank pp" dump_sink
            |> A.SinkCallbacks.add_const "overtaint check" check_ot
    in
    a
        |> A.FunctionCallbacks.add "basic start" (A.FunctionCallbacks.start_callback ~name:"basic start" ~todo)
        |> A.TraceCallbacks.add "overtaint symb update" update_symb

module PB =
    struct
        module A = A

        type p = A.t

        let name = "basic"
        let desc = "Basic controlled / uncontrolled taint analysis. Sources should be marked with the \"controlled\" or \"uncontrolled\" keyword."

        let init ?(tag = None) () =
            init_analysis ~tag "Basic Policy"
    end

module P = Policy.Make (PB)

let () = 
    Policy.register_policy (module P);
    Options.register_policy_option "basic" "-snkt" (Options.TaggedOptions.Bool(dump_taint_opt)) "Print taint bank on tainted sink.";
    Options.register_policy_option "basic" "-check-overtaint" (Options.TaggedOptions.Bool(check_overtaint_opt)) "Check if tainted registers and memory bytes would be symbolic (see symbolic policy)."
