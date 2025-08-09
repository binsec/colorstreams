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

let autokill_opt = new Options.TaggedOptions.opt false
let dontcheck_opt = new Options.TaggedOptions.opt ""
let ignore_small_reads_opt = new Options.TaggedOptions.opt (-1)
let global_fallback_opt = new Options.TaggedOptions.opt false
let local_fallback_opt = new Options.TaggedOptions.opt false
let check_locals_opt = new Options.TaggedOptions.opt true
let cloif_opt = new Options.TaggedOptions.opt "<.*>"
let croif_opt = new Options.TaggedOptions.opt "<.*>"
let deteclim_opt = new Options.TaggedOptions.opt (-1)
let debug = ref false
let suppress_sim_opt = new Options.TaggedOptions.opt false

let send_info s =
    Message.Wrap.send (Message.Wrap.Info(s))

let send_small_info s =
    if !debug
    then Message.Wrap.send (Message.Wrap.SmallInfo(s))
    else Message.Wrap.send (Message.Wrap.Debug("OOBCHECK", s))

module TT =
    struct
        type kind = Stack of string | Heap | Global of File.t * string | Unknown

        type t = 
            {
                idx : int;
                kind : kind;
                base : Address.t;
                size : int
            }

        let cnt = ref 0

        let from_source src =
            match src.Source.storage with
            | Storage.Memory(base, size) ->
            (
                cnt := !cnt + 1;
                [{idx = !cnt; kind = Unknown; base; size}, src.Source.storage]
            )
            | _ -> []

        let create_ kind base size =
            cnt := !cnt + 1;
            {idx = !cnt; kind; base; size}

        let pp_kind = function
            | Stack(var) -> Printf.sprintf "stack variable <%s>" var
            | Heap -> "heap object"
            | Global(file, var) -> Printf.sprintf "global variable <%s> from <%s>" var @@ File.get_path file
            | Unknown -> "unknown"

        let parse_kind s =
            let stackexp = Str.regexp {|stack variable <\([^>]+\)>|} in
            let globexp = Str.regexp {|global variable <\([^>]+\)> from <\([^>]+\)>|} in
            if Str.string_match stackexp s 0
            then Stack(Str.matched_group 1 s)
            else if Str.string_match globexp s 0
            then Global(File.from_file @@ Str.matched_group 2 s, Str.matched_group 1 s)
            else if s = "heap object"
            then Heap
            else if s = "unknown"
            then Unknown
            else assert false

        let pp = function 
            | Some({idx; kind; base; size}) -> Printf.sprintf "%s: %d(%s)" (pp_kind kind) idx @@ Storage.pp (Storage.Memory(base, size))
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
            let num = function
                | Some({idx; _}) -> idx
                | _ -> 0
            in
            let x = num tt in
            let y = num tt_ in
            Some(x - y)

        let equals tt tt_ =
            match compare tt tt_ with
            | Some(0) -> true
            | _ -> false

        let to_generic = function
            | Some(tt) ->
            (
                let kindres = Result.mk_string ~label:"kind" @@ pp_kind tt.kind in
                let stores = Result.mk_storage ~label:"object" (Storage.Memory(tt.base, tt.size)) in
                let idxres = Result.mk_int ~label:"id" @@ Z.of_int tt.idx in
                Result.Res([kindres; stores; idxres])
            )
            | None -> Result.Str("-")

        let of_generic = function
            | Result.Res
                (
                    [
                        {Result.result = Str(kind); _};
                        {Result.result = Sto(Storage.Memory(base, size)); _};
                        {Result.result = Int(idx); _}
                    ]
                )
                    ->
            (
                let kind = parse_kind kind in
                {kind; base; size; idx = Z.to_int idx}
            )
            | _ -> assert false
    end

module MemObjects =
    struct
        module MemMap = Map.Make(Address)

        type t = TT.t MemMap.t

        let empty =
            MemMap.empty

        let check_overlap sto objs =
            match sto with
            | Storage.Memory(addr, size) ->
            (
                let tocheck, _, _ = MemMap.split (Address.add_int addr size) objs in
                let stoi = Storage.memory_interval sto in
                let rec aux tocheck res =
                    try
                        let lastk, last = MemMap.find_last (fun _ -> true) tocheck in
                        let lasti = Interval.create ~lo:last.TT.base ~hi:(Address.add_int last.TT.base last.TT.size) in
                        if Interval.overlap stoi lasti
                        then aux (MemMap.remove lastk tocheck) (last::res)
                        else res
                    with Not_found -> res
                in
                aux tocheck []
            )
            | _ -> []

        let get_at addr objs =
            match MemMap.split addr objs with
            | _, Some(tt), _ -> Some(tt)
            | tocheck, _, _ ->
            (
                try
                    let _, last = MemMap.find_last (fun _ -> true) tocheck in
                    let lasti = Interval.create ~lo:last.TT.base ~hi:(Address.add_int last.base last.TT.size) in
                    if Interval.contains lasti addr
                    then Some(last)
                    else None
                with Not_found -> None
            )
    end

module ShadowGlobals =
    struct
        type t =
            {
                located : MemObjects.t;
                unknown : (File.t * string) list
            }

        let empty =
            {located = MemObjects.empty; unknown = []}

        let init () =
            let unknown = Trace.Gdb.get_globals @@ Trace.get_gdb () in
            {empty with unknown}

        let update g =
            let gdb = Trace.get_gdb () in
            let try_find (found, unknown) (file, var) =
                let path = File.get_path file in
                let expr1 = Printf.sprintf "&'%s'::%s" path var in
                let expr2 = Printf.sprintf "sizeof('%s'::%s)" path var in
                match (Trace.Gdb.eval_expr ~silent:true gdb expr1), Trace.Gdb.eval_expr ~silent:true gdb expr2 with
                | Some(addr), Some(size) -> ((TT.create_ (TT.Global(file, var)) addr @@ Z.to_int size)::found), unknown
                | _ -> found, (file, var)::unknown
            in
            let found, unknown = List.fold_left try_find ([], []) g.unknown in
            {located = List.fold_left (fun res tt -> MemObjects.MemMap.add tt.TT.base tt res) g.located found; unknown}
    end

module ShadowHeap =
    struct
        module IntMap = Map.Make (Int)

        type t =
            {
                heap : MemObjects.t;
                valid : bool IntMap.t
            }

        let empty =
            {heap = MemObjects.empty; valid = IntMap.empty}

        let check_valid tt h =
            assert (tt.TT.kind = TT.Heap);
            try
                IntMap.find tt.TT.idx h.valid
            with Not_found -> false

        let alloc tt h =
            assert (tt.TT.kind = TT.Heap);
            assert (not @@ check_valid tt h);
            let sto = Storage.Memory(tt.TT.base, tt.TT.size) in
            match MemObjects.check_overlap sto h.heap with
            | [] -> {heap = MemObjects.MemMap.add tt.TT.base tt h.heap; valid = IntMap.add tt.TT.idx true h.valid}
            | l -> 
            (
                let l = List.fold_left (fun res e -> res ^ ", " ^ (TT.pp (Some(e)))) (TT.pp (Some(List.hd l))) @@ List.tl l in
                raise (Failure (Printf.sprintf "allocated object overlaps with <%s>" l))
            )

        let free tt h =
            assert (tt.TT.kind = TT.Heap);
            if check_valid tt h
            then {heap = MemObjects.MemMap.remove tt.TT.base h.heap; valid = IntMap.remove tt.TT.idx h.valid}
            else raise (Failure ("double free"))

        let fallback_free addr h =
            match MemObjects.get_at addr h.heap with
            | Some(tt) ->
            (
                if tt.TT.base = addr
                then free tt h
                else raise (Failure "address does not point to base")
            )
            | _ -> raise (Failure "address does not point to object")
    end

module ResVal =
    struct
        module TTVal =
            struct
                type t = TT.t

                let to_generic r = 
                    TT.to_generic (Some(r))
            end

        module TTRes = Result.Make(TTVal)

        type wdata = AfterUpdate | Expr of Expr.t | Pattern of Expr.t | Input

        let pp_wdata = function
            | AfterUpdate
            | Expr(_) -> "direct"
            | Pattern(_) -> "pattern"
            | Input -> "input"

        type rw = Read | Write of wdata

        type reason = TagOOB of TT.t | TagUAF of TT.t | Mapping of string | Heap of string | Global of string | Stack of string | Other

        type t =
            {
                id : int;
                rw : rw;
                valid : bool;
                loc : Trace.Gdb.LocRes.result option Lazy.t;
                simple_loc : (string * Address.t option) option;
                target : Storage.t;
                base : Expr.t;
                size : Expr.t;
                reason : reason
            }

        let cnt = ref 0

        let create func addr rw valid target base size reason =
            let id = !cnt in
            cnt := !cnt + 1;
            let loc = lazy (Trace.Gdb.get_location @@ Trace.get_gdb ()) in
            let simple_loc = Some(func, addr) in
            {id; rw; valid; loc; simple_loc; target; base; size; reason}

        let instr_rw_to_rw ?(wdata = AfterUpdate) = function
            | Instruction.Operand.Read -> Read
            | Instruction.Operand.Both
            | Instruction.Operand.Write -> Write(wdata)

        let pp_rw = function
            | Read -> "read"
            | Write(_) -> "write"

        let similar ?(strict = false) a b =
            let rwcheck =
                match a.rw, b.rw with
                | Read, Read
                | Write(_), Write(_) -> true
                | _ -> false
            in
            let loccheck =
                let check_entries a b =
                    a.Trace.Gdb.Location.func = b.Trace.Gdb.Location.func && a.line = b.line && a.file = b.file
                in
                let rec check a b =
                    if check_entries a b
                    then
                    (
                        if not strict 
                        then true
                        else
                        (
                            match a.caller, b.caller with
                            | Some(a), Some(b) -> check a b
                            | None, None -> true
                            | _ -> false
                        )
                    )
                    else false
                in
                if strict || a.simple_loc = None || b.simple_loc = None
                then
                (
                    match a.loc, b.loc with
                    | lazy (Some(aloc)), lazy (Some(bloc)) -> check aloc.Result.result bloc.Result.result
                    | _ -> false
                )
                else a.simple_loc = b.simple_loc
            in
            let reasoncheck =
                if strict
                then
                (
                    match a.reason, b.reason with
                    | TagOOB(tta), TagOOB(ttb)
                    | TagUAF(tta), TagUAF(ttb) -> tta.kind = ttb.kind
                    | ra, rb -> ra = rb
                )
                else true
            in
            rwcheck && loccheck && reasoncheck

        let of_generic id = function
            | Result.Res
                (
                    [
                        {Result.result = Result.Sto(target); _}; 
                        {Result.result = Str(rw); _}; 
                        {Result.result = Bool(valid); _}; 
                        {Result.result = Res({Result.result = Str(reasontype); _}::reason); _};
                        loc
                    ]
                ) 
                    ->
            (
                let rw =
                    match rw with
                    | "read" -> Read
                    | "write" -> Write(AfterUpdate)
                    | _ -> assert false
                in
                let reason =
                    match reasontype, reason with
                    | "out-of-bounds", [reason] -> TagOOB(TT.of_generic reason.result)
                    | "use-after-free", [reason] -> TagUAF(TT.of_generic reason.result)
                    | "memory mapping violation", [{result = Str(s); _}] -> Mapping(s)
                    | "heap violation", [{result = Str(s); _}] -> Heap(s)
                    | "global variable violation", [{result = Str(s); _}] -> Global(s)
                    | "stack violation", [{result = Str(s); _}] -> Stack(s)
                    | "could not be ruled out", [] -> Other
                    | _ -> assert false
                in
                let loc = lazy
                    (
                        match loc.Result.result with
                        | Res(_) -> Some(Trace.Gdb.get_location_from_generic_result loc)
                        | Str("not found") -> None
                        | _ -> assert false
                    )
                in
                ignore @@ Lazy.force loc;
                let simple_loc = None in
                let base, size = 
                    match target with
                    | Storage.Memory(addr, size) -> Expr.Const(addr, 8), Expr.Const(Z.of_int size, 8)
                    | _ -> assert false
                in
                {id; rw; valid; loc; simple_loc; target; base; size; reason}
            )
            | _ -> assert false

        let to_generic r =
            let rwres = Result.mk_string ~label:"r/w" @@ pp_rw r.rw in
            let validres = Result.mk_bool ~label:"valid" r.valid in
            let locres = 
                match r.loc with
                | lazy (Some(loc)) -> Trace.Gdb.LocRes.export loc 
                | _ -> Result.mk_string ~label:"location" "not found"
            in
            let targetres = Result.mk_storage ~label:"target" r.target in
            let mk_string_reason name msg =
                let typeres = Result.mk_string ~label:"type" name in
                let descres = Result.mk_string ~label:"description" msg in
                Result.mk_res_list ~label:"reason" [typeres; descres]
            in
            let reasonres =
                match r.reason with
                | TagOOB(tt) ->
                (
                    let typeres = Result.mk_string ~label:"type" "out-of-bounds" in
                    let ttres = TTRes.export @@ TTRes.create ~label:"object" tt in
                    Result.mk_res_list ~label:"reason" [typeres; ttres]
                )
                | TagUAF(tt) ->
                (
                    let typeres = Result.mk_string ~label:"type" "use-after-free" in
                    let ttres = TTRes.export @@ TTRes.create ~label:"object" tt in
                    Result.mk_res_list ~label:"reason" [typeres; ttres]
                )
                | Mapping(s) -> mk_string_reason "memory mapping violation" s
                | Heap(s) -> mk_string_reason "heap violation" s
                | Global(s) -> mk_string_reason "global variable violation" s
                | Stack(s) -> mk_string_reason "stack violation" s
                | Other -> Result.mk_string ~label:"reason" "could not be ruled out"
            in
            Result.Res([targetres; rwres; validres; reasonres; locres])
    end

module Res = 
    struct
        include Result.Make(ResVal)

        let create_detection r =
            let label = Printf.sprintf "Detection %d" r.ResVal.id in
            create ~label r

        let from_generic_result r =
            let label = r.Result.label in
            let idexp = Str.regexp {|Detection \([0-9]+\)|} in
            assert (Str.string_match idexp label 0);
            let id = int_of_string @@ Str.matched_group 1 label in
            create ~label:r.Result.label @@ ResVal.of_generic id r.Result.result
    end

class mh_detec_filter report_cnt filter =
    object(self)
        inherit Message.handler () as super

        method handle msg =
            match msg.Message.ext_value with
            | Result.Result(Result.Custom(Res.CustomResult(detec), _)) ->
            (
                if filter detec
                then 
                (
                    report_cnt#incr;
                    super#handle msg
                )
                else 
                (
                    send_small_info (lazy (Printf.sprintf "suppressed detection %d" detec.Result.result.ResVal.id));
                    None
                )
            )
            | _ -> super#handle msg
    end

let default_filter ~min_read_size ~reads_only_in ?(suppress_similar = None) detec =
    let ok_read =
        match detec.Result.result.ResVal.rw, detec.result.reason with
        | ResVal.Read, TagOOB(tt) ->
        (
            let targi = Storage.memory_interval detec.result.target in
            let tti = Interval.create ~lo:tt.TT.base ~hi:(Address.add_int tt.base tt.size) in
            let sizecheck =
                try
                    let prev, next = Interval.sub targi tti in
                    let get_card = function
                        | Some(itv) -> Interval.card itv
                        | None -> Z.zero
                    in
                    Z.gt (Z.add (get_card prev) @@ get_card next) @@ Z.of_int min_read_size
                with _ -> Z.gt (Z.add (Interval.card targi) @@ Interval.dist targi tti) @@ Z.of_int min_read_size
            in
            let funccheck =
                let fname = 
                    match detec.result.simple_loc, detec.result.loc with
                    | Some(func, _), _ -> func
                    | _, lazy (Some({result = {func = Some(fname); _}; _})) -> fname
                    | _ -> "???"
                in
                Str.string_match reads_only_in fname 0
            in
            sizecheck && funccheck
        )
        | _ -> true
    in
    let ok_sim = lazy
        (
            match suppress_similar with
            | Some(reported) -> 
            (
                try
                    let sim = List.find (fun e -> ResVal.similar ~strict:false e.Result.result detec.Result.result) !reported in
                    if Storage.size detec.result.ResVal.target > Storage.size sim.result.target
                    then 
                    (
                        ignore @@ Lazy.force detec.result.loc;
                        reported := detec::!reported;
                        true
                    )
                    else false
                with Not_found ->
                (
                    ignore @@ Lazy.force detec.result.loc;
                    reported := detec::!reported;
                    true
                )
            )
            | _ -> true
        )
    in
    ok_read && Lazy.force ok_sim

let default_filter ~min_read_size ~reads_only_in ~suppress_similar =
    let suppress_similar = if suppress_similar then Some(ref []) else None in
    default_filter ~min_read_size ~reads_only_in ~suppress_similar

module S =
    struct
        module SStats =
            struct
                type t =
                    {
                        detec_cnt : Stats.counter;
                        report_cnt : Stats.counter;
                        heap_errs : Stats.counter
                    }

                let create () =
                    let detec_cnt = new Stats.counter "detec_cnt" "Detections" 0 in
                    let report_cnt = new Stats.counter "report_cnt" "Reported" 0 in
                    let heap_errs = new Stats.counter "heap_errs" "Heap errors" 0 in
                    {detec_cnt; report_cnt; heap_errs}

                let add_to_bundle s b =
                    b
                        |> Stats.Bundle.add_stat (s.detec_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (s.report_cnt :> Stats.stat)
                        |> Stats.Bundle.add_stat (s.heap_errs :> Stats.stat)
            end

        type t =
            {
                sheap : ShadowHeap.t;
                sglobals : ShadowGlobals.t;
                locals : (string * Address.t * int) list Lazy.t;
                stop_check : bool;
                dontcheck : Str.regexp;
                global_fallback : bool;
                local_fallback : bool;
                check_locals : bool;
                cloif : Str.regexp;
                croif : Str.regexp;
                deteclim : int;
                suppress_similar : bool;
                stop_malloc : bool;
                maybe_var_ref : Storage.t list;
                autokill : bool;
                filter : Message.handler option;
                stats : SStats.t
            }

        let create ?(tag = None) () =
            let sheap = ShadowHeap.empty in
            let sglobals = ShadowGlobals.empty in
            let locals = lazy [] in
            let stop_check = false in
            let dontcheck = Str.regexp {|$|} in
            let global_fallback = false in
            let local_fallback = false in
            let check_locals = true in
            let cloif = Str.regexp ".*" in
            let croif = Str.regexp ".*" in
            let deteclim = (-1) in
            let suppress_similar = false in
            let stop_malloc = false in
            let maybe_var_ref = [] in
            let autokill = false in
            let stats = SStats.create () in
            let filter = None in
            {sheap; sglobals; locals; stop_check; dontcheck; global_fallback; local_fallback; check_locals; cloif; croif; deteclim; suppress_similar; stop_malloc; maybe_var_ref; autokill; filter; stats}

        let add_stats_to_bundle b s =
            SStats.add_to_bundle s.stats b

        let set_autokill autokill s =
            {s with autokill}

        let set_filter filter s =
            match filter with
            | Some(filter) -> {s with filter = Some(new mh_detec_filter s.stats.report_cnt filter)}
            | None -> {s with filter = None}

        let with_filter todo s =
            match s.filter with
            | Some(filter) -> Message.with_handler todo filter
            | None -> todo ()

        let init_globals s =
            {s with sglobals = ShadowGlobals.update @@ ShadowGlobals.init ()}

        let check_valid tt s =
            ShadowHeap.check_valid tt s.sheap

        let alloc tt s =
            {s with sheap = ShadowHeap.alloc tt s.sheap}

        let free tt s =
            try
                {s with sheap = ShadowHeap.free tt s.sheap}
            with e ->
            (
                if s.autokill
                then
                ( 
                    Trace.terminate_next ();
                    Message.Wrap.send (Message.Wrap.BigInfo("stopping on invalid free"));
                )
                ;
                raise e
            )

        let fallback_free sto s =
            {s with sheap = ShadowHeap.fallback_free sto s.sheap}

        let set_stop_check stop_check s =
            {s with stop_check}

        let set_dontcheck dontcheck s =
            let dontcheck = Utils.pattern_list dontcheck in
            {s with dontcheck}

        let set_global_fallback global_fallback s =
            {s with global_fallback}

        let set_local_fallback local_fallback s =
            {s with local_fallback}

        let set_check_locals check_locals s =
            {s with check_locals}

        let set_cloif cloif s =
            {s with cloif}

        let set_croif croif s =
            {s with croif}

        let set_deteclim deteclim s =
            {s with deteclim}

        let set_suppress_similar suppress_similar s =
            {s with suppress_similar}

        let set_stop_malloc stop_malloc s =
            {s with stop_malloc}

        let find_maybe_vars instr s =
            let maybe_var_ref =
                if String.starts_with ~prefix:"lea" instr.Instruction.raw_ins
                then instr.Instruction.writes
                else []
            in
            {s with maybe_var_ref}

        let clear_maybe_vars s =
            {s with maybe_var_ref = []}

        let update_locals instr s =
            let locals = lazy
                (
                    if s.check_locals && Str.string_match s.cloif instr.Instruction.fname 0
                    then
                    (
                        let gdb = Trace.get_gdb () in
                        let get_infos res var =
                            try
                                let expr1 = Printf.sprintf "&%s" var in
                                let expr2 = Printf.sprintf "sizeof(%s)" var in
                                match (Trace.Gdb.eval_expr ~silent:true gdb expr1), Trace.Gdb.eval_expr ~silent:true gdb expr2 with
                                | Some(addr), Some(size) -> (var, addr, Z.to_int size)::res
                                | _ -> res
                            with _ -> res
                        in
                        List.fold_left get_infos [] @@ Trace.Gdb.get_locals gdb
                    )
                    else []
                )
            in
            {s with locals}

        let local_at addr_ s =
            let check_local (var, addr, size) =
                let ivar = Interval.create ~lo:addr ~hi:(Address.add_int addr size) in
                Interval.contains ivar addr_
            in
            List.find_opt check_local @@ Lazy.force s.locals

        let check_maybe_vars s =
            if s.maybe_var_ref = []
            then [], s
            else
            (
                let do_check sto res =
                    try
                        let stoval = Trace.stoval sto in
                        let loc = lazy (local_at stoval s) in
                        match (MemObjects.get_at stoval s.sglobals.located), loc with
                        | Some(tt), _ -> (sto, tt)::res
                        | _, lazy (Some(var, addr, size)) -> 
                        (
                            let tt = TT.create_ (TT.Stack(var)) addr size in
                            (sto, tt)::res
                        )
                        | _ -> res
                    with _ -> assert false
                in
                (List.fold_right do_check s.maybe_var_ref []), s
            )
    end

module TBBase = 
    struct
        include Taint.MakeBank (TT)

        let byte_bit_merge a b =
            if TT.equals a b
            then a
            else None

        let bit_merge a b =
            match a, b with
            | Some(_), None -> a
            | None, Some(_) -> b
            | _ -> byte_bit_merge a b

        let direct tt =
            tt

        let indirect tt =
            None

        let control tt = 
            None

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
            let cancel = lazy (List.init (Binsec.Dba.Expr.size_of expr) (fun _ -> None)) in
            let has_taint tt =
                match List.find_map (fun e -> e) tt with
                | Some(_) -> true
                | _ -> false
            in
            match op with
            | Binsec.Dba.Binary_op.Plus
            | Minus ->
            (
                match (has_taint ta), has_taint tb with
                | true, false -> ta
                | false, true -> tb
                | _ -> Lazy.force cancel
            )
            (*| And ->
            (
                let mask tt cst =
                    let check_bit n tt =
                        if Z.logand Z.one @@ Z.shift_right cst n = Z.one
                        then tt
                        else None
                    in
                    List.mapi check_bit tt
                in
                match a, b with
                | (Binsec.Dba.Expr.Cst(_)), Binsec.Dba.Expr.Cst(_) -> Lazy.force cancel
                | (Binsec.Dba.Expr.Cst(bv)), _ -> mask tb @@ Binsec.Bitvector.value_of bv
                | _, Binsec.Dba.Expr.Cst(bv) -> mask ta @@ Binsec.Bitvector.value_of bv
                | _ -> Lazy.force cancel
            )*)
            | Concat -> ta @ tb
            | _ -> Lazy.force cancel

        let decide instr _ b =
            if instr.Instruction.raw_ins = "syscall " || instr.Instruction.raw_ins = "rdtscp "
            then Symbolic.DbaTaint.Fallback
            else
            (
                try
                    ignore @@ List.find (fun sto -> has_taint sto b) instr.Instruction.direct_reads;
                    Symbolic.DbaTaint.Propagate
                with Not_found -> Symbolic.DbaTaint.Erase
            )

        let fallback instr b =
            let tt = 
                let tt = List.fold_left
                    (
                        fun res sto ->
                            match res, get_single_taint sto b with
                            | None, Some(taint) -> Some((Some(taint)))
                            | Some(Some(_)), Some(_) -> Some(None)
                            | _ -> res
                    )
                    None instr.Instruction.direct_reads
                in
                match tt with
                | Some(tt) -> tt
                | _ -> None
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
                        else b
                )
                b instr.Instruction.writes
    end

module TB = Symbolic.DbaTaint.Make (TBBase)
module A = Analysis.Make (TB) (S)

type t = A.t

let check_oob func addr basesto tgtsto rw addrexpr sizeexpr a =
    let state = A.get_state a in
    let stoi = Storage.memory_interval tgtsto in
    let maps = lazy (MemMaps.where_sto tgtsto @@ MemMaps.get ()) in
    let perms_ok = lazy
        (
            match Lazy.force maps with
            | [_, map] ->
            (
                let r, w =
                    match rw with
                    | ResVal.Read -> (Some(true)), None
                    | ResVal.Write(_) -> None, Some(true)
                in
                MemMaps.check_perms ~r ~w map && Interval.subset stoi map.MemMaps.range
            )
            | _ -> false
        )
    in
    let create_detection a reason =
        if state.autokill && not @@ Lazy.force perms_ok
        then
        ( 
            Trace.terminate_next ();
            Message.Wrap.send (Message.Wrap.BigInfo("stopping on memory violation"));
        )
        ;
        let detection = Res.create_detection @@ ResVal.create func addr rw (Lazy.force perms_ok) tgtsto addrexpr sizeexpr reason in
        let state = A.get_state a in
        state.S.stats.S.SStats.detec_cnt#incr;
        Res.send ~prefix:"RESULT" detection;
        if state.deteclim > 0 && state.S.stats.S.SStats.report_cnt#get > state.deteclim then Trace.terminate_next ();
        a
    in
    let msg = lazy (Printf.sprintf "checking OOB for %s to %s" (ResVal.pp_rw rw) @@ Storage.pp tgtsto) in
    match addrexpr, TB.get_single_taint basesto @@ A.get_bank a with
    | _, Some(tt) ->
    (
        send_small_info (lazy ((Lazy.force msg) ^ " (" ^ (TT.pp (Some(tt))) ^ ")"));
        let check (res, ok) tt =
            if ok
            then [], ok
            else
            (
                let tti = Interval.create ~lo:tt.TT.base ~hi:(Address.add_int tt.base tt.TT.size) in
                if tt.TT.kind = TT.Heap && not @@ S.check_valid tt state
                then ((ResVal.TagUAF(tt))::res), false
                else if not @@ Interval.subset stoi tti
                then ((ResVal.TagOOB(tt))::res), false
                else [], true
            )
        in
        let reasons, ok = List.fold_left check ([], false) [tt] in
        if ok
        then a
        else List.fold_left create_detection a reasons
    )
    (*assume that writes to stack or globals with static offsets are ok*)
    | Expr.Var(Storage.Register("rip", _)), _
    | Expr.Var(Storage.Register("rsp", _)), _ 
    | Expr.Bnop(Expr.Add, Expr.Var(Storage.Register("rip", _)), Expr.Const(_)), _
    | Expr.Bnop(Expr.Add, Expr.Var(Storage.Register("rsp", _)), Expr.Const(_)), _ -> a
    (*default to checking target is valid*)
    | _ ->
    (
        send_small_info (lazy ((Lazy.force msg) ^ " (fallback)"));
        (*let check_global () =
            let fallback () =
                match MemObjects.check_overlap sto state.sglobals.located with
                | [] -> (*create_detection ResVal.Other*) a
                | [tt] -> 
                (
                    let tti = Interval.create ~lo:tt.TT.base ~hi:(Address.add_int tt.base tt.TT.size) in
                    if Interval.subset stoi tti
                    then a
                    else create_detection a (ResVal.Global("partial object overlap"))
                )
                | _ -> create_detection a (ResVal.Global("multiple objects overlap"))
            in
            match base with
            | Some(op) -> 
            (
                let stoval = Trace.stoval @@ Storage.of_operand op in
                let base_addr = Address.add_int stoval disp in
                match MemObjects.get_at base_addr state.sglobals.located with
                | Some(tt) ->
                (
                    let msg = lazy (Printf.sprintf "base address in operand identified as within %s (%s)" (TT.pp_kind tt.TT.kind) @@ TT.pp (Some(tt))) in
                    send_small_info msg;
                    let tti = Interval.create ~lo:tt.TT.base ~hi:(Address.add_int tt.TT.base tt.TT.size) in
                    if Interval.subset stoi tti
                    then a
                    else create_detection a (ResVal.Global(Lazy.force msg))
                )
                | _ -> fallback ()
            )
            | _ -> fallback ()
        in
        let check_local () =
            match base with
            | Some(op) -> 
            (
                let stoval = Trace.stoval @@ Storage.of_operand op in
                let base_addr = Address.add_int stoval disp in
                match S.local_at base_addr state with
                | Some(var, vaddr, vsize) ->
                (
                    let msg = lazy (Printf.sprintf "base address in operand identified as within stack variable <%s> (%s)" var @@ Storage.pp (Storage.Memory(vaddr, vsize))) in
                    send_small_info msg;
                    let tti = Interval.create ~lo:vaddr ~hi:(Address.add_int vaddr vsize) in
                    if Interval.subset stoi tti
                    then a
                    else create_detection a (ResVal.Stack(Lazy.force msg))
                )
                | _ -> a
            )
            | _ -> a
        in*)
        match Lazy.force maps with
        | [] -> create_detection a (ResVal.Mapping("unmapped location"))
        | [desc, map] -> 
        (
            if Interval.subset stoi map.MemMaps.range
            then
            (
                if Lazy.force perms_ok
                then
                (
                    let in_main_file =
                        match desc with
                        | File(file) -> (File.get_path file) = Unix.realpath @@ Options.get_target ()
                        | _ -> true
                    in
                    if MemMaps.check_perms ~w:(Some(false)) map || not in_main_file
                    then a
                    else
                    (
                        match desc with
                        | Spec("[stack]") (*-> if state.local_fallback then check_local () else a*)
                        | Spec("[heap]") (*-> check_heap_overlaps ()*)
                        | Spec(_)
                        | Anon -> a
                        | _ -> (*if state.global_fallback then check_global () else*) a
                    )
                )
                else create_detection a (ResVal.Mapping("permission violation"))
            )
            else create_detection a (ResVal.Mapping("partially unmapped location"))
        )
        | _ -> create_detection a (ResVal.Mapping("multiple mappings overlap"))
    )

(*basesto is where we look for base pointer taint, baseexpr should also include indexes, static shifts etc*)
let check_oob func addr basesto tgtsto rw baseexpr sizeexpr a =
    S.with_filter (fun () -> check_oob func addr basesto tgtsto rw baseexpr sizeexpr a) @@ A.get_state a

let maybe_stop_check_until call_id a =
    let state = A.get_state a in
    if state.stop_check
    then a
    else
    (
        let check_again _ a =
            a
                |> A.get_state
                |> S.set_stop_check false
                |> A.set_state a
        in
        state
            |> S.set_stop_check true
            |> A.set_state a
            |> A.FunctionCallbacks.on_callret call_id "check again" check_again
    )

let dontcheck_callback func a =
    match func with
    | Function.Entry({fname; call_id = Some(call_id); _}) ->
    (
        if Str.string_match (A.get_state a).dontcheck fname 0
        then maybe_stop_check_until call_id a
        else a
    )
    | _ -> a

let maybe_stop_check_and_malloc_until call_id a =
    let malloc_again _ a =
        a
            |> A.get_state
            |> S.set_stop_malloc false
            |> A.set_state a
    in
    a
        |> A.get_state
        |> S.set_stop_malloc true
        |> A.set_state a
        |> maybe_stop_check_until call_id
        |> A.FunctionCallbacks.on_callret call_id "malloc again" malloc_again

module LibFunStubsBase =
    struct
        let modulename = "libfun"

        type state = t
        type action = Update of state | OnRet of string * (Function.func list -> t -> t)
        type kind = Check | Malloc | Both | Other

        let pp_kind = function
            | Check -> "OOB check"
            | Malloc -> "memory allocation"
            | Both -> "OOB check + memory allocation"
            | Other -> "other"

        let check_kind _ call_id a kind =
            let state = A.get_state a in
            let ok = 
                match kind with
                | Check -> not state.stop_check
                | Malloc -> not state.stop_malloc
                | Both -> not (state.stop_check || state.stop_malloc)
                | Other -> true
            in
            let a =
                if ok
                then
                (
                    match kind with
                    | Check -> maybe_stop_check_until call_id a
                    | Both
                    | Malloc -> maybe_stop_check_and_malloc_until call_id a
                    | Other -> a
                )
                else a
            in
            a, ok

        let act _ call_id a = function
            | Update(a) -> a
            | OnRet(name, cb) -> A.FunctionCallbacks.on_callret call_id name cb a
end

module LibFunStubs =
    struct
        include Stubs.Make (LibFunStubsBase)

        module DefaultStubs =
            struct
                module Utils = 
                    struct
                        let do_alloc name size sto ptr a =
                            try
                                if ptr = Z.zero
                                then 
                                (
                                    send_small_info (lazy (name ^ " alloc failed"));
                                    a
                                )
                                else
                                (
                                    let tt = TT.create_ TT.Heap ptr size in
                                    let msg = lazy (Printf.sprintf "%s alloc: %s" name @@ TT.pp (Some(tt))) in
                                    try
                                        let a = A.set_state a @@ S.alloc tt @@ A.get_state a in
                                        let a = A.set_bank a @@ TB.taint sto (Some(tt)) @@ A.get_bank a in
                                        send_small_info msg;
                                        a
                                    with e ->
                                    (
                                        let msg = lazy (Printf.sprintf "%s: %s" (Lazy.force msg) @@ Printexc.to_string e) in
                                        Message.Wrap.send (Analysis.Wrap.WithLoc(Message.Wrap.Warning(msg)));
                                        (A.get_state a).stats.heap_errs#incr;
                                        a
                                    )
                                )
                            with _ -> assert false

                        let do_dealloc name sto ptr a =
                            try
                                if ptr = Z.zero
                                then 
                                (
                                    send_small_info (lazy (name ^ " dealloc: null pointer"));
                                    a
                                )
                                else
                                (
                                    match TB.get_single_taint sto @@ A.get_bank a with
                                    | Some(tt) -> 
                                    (
                                        let msg = lazy (Printf.sprintf "%s dealloc: %s" name @@ TT.pp (Some(tt))) in
                                        let do_dealloc a tt =
                                            try
                                                let a = A.set_state a @@ S.free tt @@ A.get_state a in
                                                send_small_info msg;
                                                a
                                            with e ->
                                            (
                                                let msg = lazy (Printf.sprintf "%s: %s" (Lazy.force msg) @@ Printexc.to_string e) in
                                                Message.Wrap.send (Analysis.Wrap.WithLoc(Message.Wrap.Warning(msg)));
                                                (A.get_state a).stats.heap_errs#incr;
                                                a
                                            )
                                        in
                                        List.fold_left do_dealloc a [tt]
                                    )
                                    | _ ->
                                    (
                                        let msg = lazy (Printf.sprintf "%s fallback dealloc: %s" name @@ Storage.pp sto) in
                                        try
                                            let a = A.set_state a @@ S.fallback_free ptr @@ A.get_state a in
                                            send_small_info msg;
                                            a
                                        with e ->
                                        (
                                            let msg = lazy (Printf.sprintf "%s: %s" (Lazy.force msg) @@ Printexc.to_string e) in
                                            Message.Wrap.send (Analysis.Wrap.WithLoc(Message.Wrap.Warning(msg)));
                                            (A.get_state a).stats.heap_errs#incr;
                                            a
                                        )
                                    )
                                )
                            with _ -> assert false

                        let check_mem_one_way destbasesto destexpr sizeexpr rw func call_id a =
                            let size = Trace.exprval sizeexpr in
                            if size > Z.zero
                            then
                            (
                                let sto = Storage.Memory(Trace.exprval destexpr, Base.Utils.z_to_int size) in
                                check_oob func.Function.fname None destbasesto sto rw destexpr sizeexpr a
                            )
                            else a

                        let check_mem_two_way srcbasesto srcexpr destbasesto destexpr sizeexpr wdata func call_id a =
                            let size = Trace.exprval sizeexpr in
                            if size > Z.zero
                            then
                            (
                                let src = Storage.Memory(Trace.exprval srcexpr, Base.Utils.z_to_int size) in
                                let dest = Storage.Memory(Trace.exprval destexpr, Base.Utils.z_to_int size) in
                                a
                                    |> check_oob func.Function.fname None srcbasesto src ResVal.Read srcexpr sizeexpr
                                    |> check_oob func.Function.fname None destbasesto dest (ResVal.Write(wdata)) destexpr sizeexpr
                            )
                            else a
                    end

                let malloc func call_id a =
                    let alloc_size = Z.to_int @@ Trace.stoval @@ Function.get_iarg 0 func in
                    let ret = Function.get_iret 0 func in
                    let do_malloc _ a =
                        let ptr = Trace.stoval ret in
                        Utils.do_alloc "malloc" alloc_size ret ptr a
                    in
                    LibFunStubsBase.OnRet("malloc stub ret", do_malloc)

                let calloc func call_id a =
                    let alloc_size = Z.to_int @@ Z.mul (Trace.stoval @@ Function.get_iarg 0 func) @@ Trace.stoval @@ Function.get_iarg 1 func in
                    let ret = Function.get_iret 0 func in
                    let do_calloc _ a =
                        let ptr = Trace.stoval ret in
                        Utils.do_alloc "calloc" alloc_size ret ptr a
                    in
                    LibFunStubsBase.OnRet("calloc stub ret", do_calloc)

                let realloc func call_id a =
                    let sto = Function.get_iarg 0 func in
                    let ptr = Trace.stoval sto in
                    let alloc_size = Z.to_int @@ Trace.stoval @@ Function.get_iarg 1 func in
                    let ret = Function.get_iret 0 func in
                    if ptr = Z.zero
                    then 
                    (
                        let do_realloc _ a =
                            let ptr = Trace.stoval ret in
                            Utils.do_alloc "realloc" alloc_size ret ptr a
                        in
                        LibFunStubsBase.OnRet("realloc stub alloc ret", do_realloc)
                    )
                    else
                    (
                        let do_realloc _ a =
                            let nptr = Trace.stoval ret in
                            if nptr = Z.zero
                            then
                            (
                                send_small_info (lazy "realloc failed");
                                a
                            )
                            else
                            (
                                let a = Utils.do_dealloc "realloc" sto ptr a in
                                Utils.do_alloc "realloc" alloc_size ret nptr a
                            )
                        in
                        LibFunStubsBase.OnRet("realloc stub ret", do_realloc)
                    )

                let free func call_id a =
                    let reg = Function.get_iarg 0 func in
                    let ptr = Trace.stoval reg in
                    LibFunStubsBase.Update(Utils.do_dealloc "free" reg ptr a)

                let memcpy func call_id a =
                    let srcsto = Function.get_iarg 1 func in
                    let srcexpr = Expr.Var(srcsto) in
                    let deststo = Function.get_iarg 0 func in
                    let destexpr = Expr.Var(deststo) in
                    let sizesto = Function.get_iarg 2 func in
                    let size = Trace.stoval sizesto in
                    let sizeexpr = Expr.Var(sizesto) in
                    let datasto = Storage.Memory(Trace.exprval srcexpr, Base.Utils.z_to_int size) in
                    let wdata = ResVal.Expr(Expr.Var(datasto)) in
                    LibFunStubsBase.Update(Utils.check_mem_two_way srcsto srcexpr deststo destexpr sizeexpr wdata func call_id a)

                let memset func call_id a =
                    let deststo = Function.get_iarg 0 func in
                    let destexpr = Expr.Var(deststo) in
                    let sizesto = Function.get_iarg 2 func in
                    let sizeexpr = Expr.Var(sizesto) in
                    let pattern = Function.get_iarg 1 func in
                    let wdata = ResVal.Pattern(Expr.Var(pattern)) in
                    LibFunStubsBase.Update(Utils.check_mem_one_way deststo destexpr sizeexpr (ResVal.Write(wdata)) func call_id a)

                let fread func call_id a =
                    let deststo = Function.get_iarg 0 func in
                    let destexpr = Expr.Var(deststo) in
                    let bsize = Function.get_iarg 1 func in
                    let nblocks = Function.get_iarg 2 func in
                    let sizeexpr = Expr.Bnop(Expr.Mul, (Expr.Var(nblocks)), Expr.Var(bsize)) in
                    LibFunStubsBase.Update(Utils.check_mem_one_way deststo destexpr sizeexpr (ResVal.Write(ResVal.Input)) func call_id a)

                let strcpy func call_id a =
                    let deststo = Function.get_iarg 0 func in
                    let destexpr = Expr.Var(deststo) in
                    let srcsto = Function.get_iarg 1 func in
                    let srcexpr = Expr.Var(srcsto) in
                    let srcaddr = Trace.exprval srcexpr in
                    let size = Analysis.Utils.strlen srcaddr in
                    let sizeexpr = Expr.Unop(Expr.Strlen(size), Expr.Var(srcsto)) in
                    let sizeexpr = Expr.Bnop(Expr.Add, sizeexpr, Expr.Const(Z.one, 8)) in
                    let datasto = Storage.Memory(srcaddr, Z.to_int size) in
                    let wdata = ResVal.Expr(Expr.Var(datasto)) in
                    LibFunStubsBase.Update(Utils.check_mem_two_way srcsto srcexpr deststo destexpr sizeexpr wdata func call_id a)

                let strncpy func call_id a =
                    let deststo = Function.get_iarg 0 func in
                    let destexpr = Expr.Var(deststo) in
                    let srcsto = Function.get_iarg 1 func in
                    let srcexpr = Expr.Var(srcsto) in
                    let srcaddr = Trace.exprval srcexpr in
                    let sizesto = Function.get_iarg 2 func in
                    let size = Trace.stoval sizesto in
                    let sizeexpr = Expr.Var(sizesto) in
                    let datasto = Storage.Memory(srcaddr, min (Z.to_int size) @@ Z.to_int @@ Analysis.Utils.strlen srcaddr) in
                    let wdata = ResVal.Expr(Expr.Var(datasto)) in
                    LibFunStubsBase.Update(Utils.check_mem_one_way deststo destexpr sizeexpr (ResVal.Write(wdata)) func call_id a)

                let strcat func call_id a =
                    let deststo = Function.get_iarg 0 func in
                    let destaddr = Trace.stoval deststo in
                    let srcsto = Function.get_iarg 1 func in
                    let srcaddr = Trace.stoval srcsto in
                    let size = Analysis.Utils.strlen srcaddr in
                    let sizeexpr = Expr.Unop(Expr.Strlen(size), Expr.Var(srcsto)) in
                    let sizeexpr = Expr.Bnop(Expr.Add, sizeexpr, Expr.Const(Z.one, 8)) in
                    let datasto = Storage.Memory(srcaddr, Z.to_int size) in
                    let wdata = ResVal.Expr(Expr.Var(datasto)) in
                    if size > Z.zero
                    then
                    (
                        let shiftsize = Analysis.Utils.strlen destaddr in
                        let isize = Z.to_int @@ Z.add size Z.one in
                        let destexpr = Expr.Bnop(Expr.Add, Expr.Var(deststo), Expr.Unop(Expr.Strlen(shiftsize), Expr.Var(deststo))) in
                        let dest = Storage.Memory(Z.add destaddr shiftsize, isize) in
                        let srcexpr = Expr.Var(srcsto) in
                        let src = Storage.Memory(srcaddr, isize) in
                        let a = a
                            |> check_oob func.Function.fname None srcsto src ResVal.Read srcexpr sizeexpr
                            |> check_oob func.Function.fname None deststo dest (ResVal.Write(wdata)) destexpr sizeexpr
                        in
                        LibFunStubsBase.Update(a)
                    )
                    else LibFunStubsBase.Update(a)

                let _ =
                    let register name desc kind stub =
                        register name [] desc kind @@ basic_parsr stub
                    in
                    let register_default fname name =
                        let kind, parsr = get_parsr name in
                        register_default fname name kind @@ parsr []
                    in

                    (*default memory allocation*)
                    register "default_malloc" "libc malloc behaviour" Malloc malloc;
                    register_default "malloc" "default_malloc";
                    register_default "malloc@plt" "default_malloc";
                    register "default_calloc" "libc calloc behaviour" Malloc calloc;
                    register_default "calloc" "default_calloc";
                    register_default "calloc@plt" "default_calloc";
                    register_default "__libc_calloc" "default_calloc";
                    register "default_realloc" "libc realloc behaviour" Malloc realloc;
                    register_default "realloc" "default_realloc";
                    register_default "realloc@plt" "default_realloc";
                    register_default "__libc_realloc" "default_realloc";
                    register "default_free" "libc free behaviour" Malloc free;
                    register_default "free" "default_free";
                    register_default "free@plt" "default_free";
                    register_default "cfree" "default_free"; (*deprecated but just in case*)

                    (*default library functions*)
                    (*memcpy & co*)
                    register "default_memcpy" "libc memcpy behaviour" Check memcpy;
                    register_default "memcpy" "default_memcpy";
                    register_default "memcpy@plt" "default_memcpy";
                    register_default "memmove" "default_memcpy";
                    register_default "memmove@plt" "default_memcpy";
                    register "default_memset" "libc memset behaviour" Check memset;
                    register_default "memset" "default_memset";
                    register_default "memset@plt" "default_memset";
                    (*files*)
                    register "default_fread" "libc fread behaviour" Check fread;
                    register_default "fread" "default_fread";
                    register_default "fread@plt" "default_fread";
                    (*strings*)
                    register "default_strcpy" "libc strcpy behaviour" Check strcpy;
                    register_default "strcpy" "default_strcpy";
                    register_default "strcpy@plt" "default_strcpy";
                    register_default "stpcpy" "default_strcpy";
                    register_default "stpcpy@plt" "default_strcpy";
                    register "default_strncpy" "libc strncpy behaviour" Check strncpy;
                    register_default "strncpy" "default_strncpy";
                    register_default "strncpy@plt" "default_strncpy";
                    register "default_strcat" "libc strcat behaviour" Check strcat;
                    register_default "strcat" "default_strcat";
                    register_default "strcat@plt" "default_strcat"
            end
    end

let vars_callback instr a =
    let state = A.get_state a in
    if state.stop_check
    then a
    else
    (
        let hits, state = S.check_maybe_vars state in
        let process_hit (sto, tt) a =
            send_small_info (lazy (Printf.sprintf "%s references %s" (Storage.pp sto) @@ TT.pp (Some(tt))));
            a
                |> A.get_bank
                |> TB.taint sto (Some(tt))
                |> A.set_bank a 
        in
        state
            |> S.clear_maybe_vars
            |> S.find_maybe_vars instr
            |> A.set_state a
            |> List.fold_right process_hit hits
    )

let propag_callback instr a =
    A.set_bank a @@ TB.propagation instr @@ A.get_bank a

let check_callback instr a =
    let state = A.get_state a in
    if state.stop_check
    then a
    else List.fold_left
        (
            fun a op ->
                match op.Instruction.Operand.kind with
                | Instruction.Operand.Component(Instruction.Operand.Base, Instruction.Operand.Memory(mem), rw) ->
                (
                    let basesto = Instruction.Operand.to_storage op in
                    let sto = Storage.Memory(mem.Instruction.Operand.addr, mem.size) in
                    let baseexpr = Instruction.Operand.mem_expr mem in
                    let sizeexpr = Expr.Const(Z.of_int mem.size, 8) in
                    check_oob instr.Instruction.fname (Some(instr.Instruction.address)) basesto sto (ResVal.instr_rw_to_rw rw) baseexpr sizeexpr a
                )
                | _ -> a
        )   
        a instr.Instruction.operands

let init_analysis 
    ?(register_stats = true)
    ?autokill 
    ?dontcheck
    ?global_fallback
    ?local_fallback
    ?check_locals
    ?ignore_small_reads
    ?cloif
    ?croif
    ?deteclim
    ?suppress_similar
    ?filter 
    ?stubs
    ?(tag = None)
        name =
    let autokill = autokill_opt#maybe_get ?tag autokill in
    let dontcheck =
        match dontcheck with
        | Some(dontcheck) -> dontcheck
        | None -> String.split_on_char ';' @@ dontcheck_opt#get ?tag
    in
    let global_fallback = global_fallback_opt#maybe_get ?tag global_fallback in
    let local_fallback = local_fallback_opt#maybe_get ?tag local_fallback in
    let check_locals = check_locals_opt#maybe_get ?tag check_locals in
    let cloif = Utils.pattern_list @@ String.split_on_char ';' @@ cloif_opt#maybe_get ?tag cloif in
    let croif = Utils.pattern_list @@ String.split_on_char ';' @@ croif_opt#maybe_get ?tag croif in
    let deteclim = deteclim_opt#maybe_get ?tag deteclim in
    let suppress_similar = suppress_sim_opt#maybe_get ?tag suppress_similar in
    let filter =
        match filter with
        | Some(filter) -> filter
        | None -> default_filter ~min_read_size:(ignore_small_reads_opt#maybe_get ?tag ignore_small_reads) ~reads_only_in:croif ~suppress_similar
    in
    let stubs =
        match stubs with
        | Some(stubs) -> stubs
        | None -> LibFunStubs.create @@ LibFunStubs.cli_specs#get ?tag
    in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    if register_stats
    then Stats.register ~silent:true @@ S.SStats.add_to_bundle (A.get_state a).stats @@ A.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ A.get_name a) a
    ;
    let todo a =
        a
            |> A.get_state 
            |> S.set_autokill autokill
            |> S.set_filter (Some(filter))
            |> S.init_globals
            |> S.set_dontcheck dontcheck
            |> S.set_global_fallback global_fallback
            |> S.set_local_fallback local_fallback
            |> S.set_check_locals check_locals
            |> S.set_cloif cloif
            |> S.set_croif croif
            |> S.set_deteclim deteclim
            |> S.set_suppress_similar suppress_similar
            |> A.set_state a
            |> A.FunctionCallbacks.add ~priority:100 "stubs" (LibFunStubs.run stubs)
            |> A.FunctionCallbacks.add ~priority:50 "dontcheck" dontcheck_callback
            |> A.InstructionCallbacks.add ~priority:100 "locals" (fun instr a -> A.set_state a @@ S.update_locals instr @@ A.get_state a)
            |> A.InstructionCallbacks.add ~priority:90 "vars" vars_callback
            |> A.InstructionCallbacks.add ~priority:75 "check" check_callback
            |> A.InstructionCallbacks.add ~priority:50 "propagation" propag_callback
    in
    A.FunctionCallbacks.add "oob check start" (A.FunctionCallbacks.start_callback ~name:"oob check start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "oobcheck"
        let desc = "OOB checker based on taint."

        let init ?(tag = None) () =
            init_analysis ~tag "OOB Checker"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option "oobcheck" "-autokill" (Options.TaggedOptions.Bool(autokill_opt)) "Exit automatically when a crash is unavoidable.";
    Options.register_policy_alias "oobcheck" "-autokill" "-ak";
    Options.register_policy_option "oobcheck" "-dont-check" (Options.TaggedOptions.String(dontcheck_opt)) "Don't check for OOBs inside these functions (format: name;<regexp>;...).";
    Options.register_policy_alias "oobcheck" "-dont-check" "-dc";
    LibFunStubs.register_options "oobcheck";
    Options.register_policy_option "oobcheck" "-ignore-small-reads" (Options.TaggedOptions.Int(ignore_small_reads_opt)) "Ignore OOB reads smaller than the given number of bytes (useful to suppress spurious detections in vectorized library functions).";
    Options.register_policy_option "oobcheck" "-global-fallback" (Options.TaggedOptions.Bool(global_fallback_opt)) "Enable fallback checks for global variables.";
    Options.register_policy_option "oobcheck" "-local-fallback" (Options.TaggedOptions.Bool(local_fallback_opt)) "Enable fallback checks for local variables.";
    Options.register_policy_option "oobcheck" "-dont-check-locals" (Options.TaggedOptions.Bool(check_locals_opt)) "Disable local variable checks.";
    Options.register_policy_option "oobcheck" "-debug" (Options.TaggedOptions.Untagged(fun () -> debug := true)) "Show debug messages with verbosity >= 2.";
    Options.register_policy_option PB.name "-check-locals-only-in-func" (Options.TaggedOptions.String(cloif_opt)) "Only check local variable bounds for those in the specified functions.";
    Options.register_policy_alias PB.name "-check-locals-only-in-func" "-cloif";
    Options.register_policy_option PB.name "-check-reads-only-in-func" (Options.TaggedOptions.String(croif_opt)) "Only check reads in specified functions.";
    Options.register_policy_alias PB.name "-check-reads-only-in-func" "-croif";
    Options.register_policy_option PB.name "-detection-limit" (Options.TaggedOptions.Int(deteclim_opt)) "Stop analysis after n detections.";
    Options.register_policy_option PB.name "-suppress-similar" (Options.TaggedOptions.Bool(suppress_sim_opt)) "Only report the first of a set of similar detections."
