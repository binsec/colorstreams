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

type 'a ok = OK of 'a | KO of 'a | MsgKO of string * Stats.Bundle.t option
type propres = 
    {
        proj : StateFormula.Projections.Projection.t;
        result : Result.wrapped_result ok
    }

let mk_ok_and_log prop proj sf ok =
    match sf with
    | Some(sf) ->
    (
        let fname = 
            (StateFormula.Projections.Projection.pp proj) ^ "_" ^ prop ^
            (
                match ok with
                | OK(_) -> "_OK"
                | _ -> "_KO"
            )
        in
        let file = File.create ~keep:true fname ".smt2" in
        File.write (StateFormula.pp sf) file;
        ok
    )
    | _ -> ok

let mk_ok prop proj sf res =
    mk_ok_and_log prop proj sf (OK(res))

let mk_ko prop proj sf res =
    mk_ok_and_log prop proj sf (KO(res))

let mk_ko_msg ?(stats = None) prop proj sf msg =
    mk_ok_and_log prop proj sf (MsgKO(msg, stats))

module PropResVal =
    struct
        type t = propres

        let to_generic v =
            let projres = Result.mk_string ~label:"projection" @@ StateFormula.Projections.Projection.pp v.proj in
            let rres =
                match v.result with
                | OK(res) -> 
                (
                    let res = Result.to_generic res in
                    {res with Result.label = "result"}
                )
                | KO(res) ->
                (
                    let res = Result.to_generic res in
                    {res with Result.label = "error"}
                )
                | MsgKO(msg, stats) -> Result.mk_string ~label:"error" ~stats msg
            in
            Result.Res([projres; rres])
    end

module PropRes = Result.Make(PropResVal)

type t =
    {
        name : string;
        desc : string;
        check : StateFormula.Projections.Projection.t -> StateFormula.t -> PropRes.result;
        stats : StateFormula.SolverStats.t option
    }

module StringMap = Map.Make (String)

let props = ref StringMap.empty

let register name desc check stats =
    (
        match stats with 
        | Some(stats) -> Stats.register stats.StateFormula.SolverStats.b
        | None -> ()
    );
    let check proj sf =
        let result = check proj sf in
        PropRes.create ~label:name {proj; result}
    in
    props := StringMap.add name {name; desc; check; stats} !props

let get name =
    StringMap.find name !props

let get_list () =
    List.rev @@ StringMap.fold (fun key p res -> (key, p.desc)::res) !props []

let pp () =
    let pp_ res (name, desc) =
        Printf.sprintf "%s%-20s%s\n" res ("- " ^ name) desc
    in
    let msg = List.fold_left pp_ "Available symbolic properties:\n" @@ get_list () in
    Message.Wrap.send (Message.Wrap.BigInfo(msg))

let incr stats res =
    let status = res.Result.result in
    StateFormula.SolverStats.incrres status stats;
    match res.stats with
    | Some(b) ->
        let solv = List.hd @@ List.rev @@ Str.split (Str.regexp ": ") (Stats.Bundle.get_stat "solver" b)#pp in
        StateFormula.SolverStats.incrsolv solv stats
    | _ -> ()

let check_on_file arg =
    let path, projfinder, props =
        match String.split_on_char ' ' arg with
        | [path; projname; props] -> path, (fun proj -> projname = Identifier.pp_basic @@ StateFormula.Projections.Projection.get_id proj), props
        | [path; props] -> path, (fun proj -> true), props
        | _ -> raise (Failure(Printf.sprintf "property check: invalid formatting <%s>" arg))
    in
    let sf = StateFormula.from_file @@ File.from_file path in
    let proj, _ = 
        try
            StateFormula.Projections.find_first projfinder @@ StateFormula.get_projections sf 
        with Not_found -> raise (Failure("property check: projection not found"))
    in
    let props = List.fold_left
        (
            fun props prop ->
                try
                    (get prop)::props
                with Not_found -> 
                (
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printf.sprintf "unknown property <%s>" prop)));
                    props
                )
        )
        []
        @@ String.split_on_char ',' props 
    in
    let res = List.map (fun prop -> PropRes.wrap @@ prop.check proj sf) props in
    Result.send ~verbosity:0 ~prefix:"RESULT" (Result.Mixed("Property checks", res))

let _ =
    Options.register_option ("-load-and-check-property", Arg.String(fun arg -> Options.register_todo (fun () -> check_on_file arg)), "Load an SMT fromula from file and check properties (format: path( projection) prop1,prop2,...)");
    Options.register_option ("-list-symbolic-properties", Arg.Unit(fun () -> Options.register_todo pp), "List available symbolic properties.")
