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

type desc = Spec of string | File of File.t | Anon

type entry =
    {
        range : Interval.t;
        permr : bool;
        permw : bool;
        permx : bool;
        foffset : int;
        desc : desc
    }

type t = (desc * entry) list

let entryexp = Str.regexp {|^\([0-9a-fA-F]*\)-\([0-9a-fA-F]*\) \(r\|-\)\(w\|-\)\(x\|-\). \([0-9a-fA-F]*\) [^ ]* [^ ]*[ ]*\(.*\)$|}

let last = ref None

let get file =
    let parse_line s =
        try
            if Str.string_match entryexp s 0
            then
            (
                let lo = Z.of_string ("0x" ^ (Str.matched_group 1 s)) in
                let hi = Z.of_string ("0x" ^ (Str.matched_group 2 s)) in
                let range = Interval.create ~lo ~hi in
                let permr = ((Str.matched_group 3 s) = "r") in
                let permw = ((Str.matched_group 4 s) = "w") in
                let permx = ((Str.matched_group 5 s) = "x") in
                let foffset = int_of_string ("0x" ^ (Str.matched_group 6 s)) in
                let desc =
                    let desc = Str.matched_group 7 s in
                    if desc = ""
                    then Anon
                    else if String.starts_with ~prefix:"[" desc
                    then Spec(desc)
                    else
                    (
                        try
                            File(File.from_file desc)
                        with _ -> Spec(desc)
                    )
                in
                {range; permr; permw; permx; foffset; desc}
            )
            else raise (Failure "mismatch")
        with e -> raise (Failure (Printf.sprintf "could not parse <%s> (%s)" s @@ Printexc.to_string e))
    in
    let lines = File.read file in
    List.map (fun e -> e.desc, e) @@ List.map parse_line lines

let get () =
    let update file lastmod =
        let maps = get file in
        (*does not work, too small resolution (second)?*)
        (*last := Some(file, lastmod, maps);*)
        maps
    in
    match !last with
    | Some(file, lastmod, maps) ->
    (
        let fstats = Unix.stat @@ File.get_path file in
        if lastmod = fstats.Unix.st_mtime
        then maps
        else update file fstats.st_mtime
    )
    | None -> 
    (
        try
            let pid = Trace.get_pid () in
            assert (pid >= 0);
            let file = File.from_file @@ Printf.sprintf "/proc/%d/maps" pid in
            update file (Unix.stat @@ File.get_path file).st_mtime
        with e ->
        (
            Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printf.sprintf "could not read memory mappings (%s)" @@ Printexc.to_string e)));
            []
        )
    )

let where_sto sto m =
    let stoi = Storage.memory_interval sto in
    List.filter (fun (_, e) -> Interval.overlap e.range stoi) m

let where_file file m =
    let file = File.from_file file in 
    let filter = function
        | File(f), _ -> (File.get_name f) = File.get_name file
        | _ -> false
    in
    List.filter filter m

let first_file m =
    let first = ref None in
    let filter = function
        | File(f), _ -> 
        (
            match !first with
            | None -> 
            (
                first := Some(File.get_name f);
                true
            )
            | Some(file) -> file = File.get_name f
        )
        | _ -> false
    in
    List.filter filter m

let check_perms ?(r = None) ?(w = None) ?(x = None) e =
    let check perm = function
        | Some(b) -> perm = b
        | None -> true
    in
    (check e.permr r) && (check e.permw w) && (check e.permx x)

let get_with_perms ?(r = None) ?(w = None) ?(x = None) m =
    List.filter (fun (_, e) -> check_perms ~r ~w ~x e) m

let entry_to_generic e =
    let rangeres = Result.mk_interval ~label:"range" e.range in
    let permres = Result.mk_string ~label:"permissions"
        (
            (if e.permr then "R" else "-") ^ 
            (if e.permw then "W" else "-") ^
            (if e.permx then "X" else "-")
        )
    in
    Result.Res([rangeres; permres])

let pp_entry_desc e =
    match e.desc with
    | Spec(s) -> s
    | File(file) -> Printf.sprintf "%s + %d" (File.get_path file) e.foffset
    | Anon -> "anonymous"

let is_kernel_space e =
    Z.geq e.range.Interval.lo @@ Z.shift_left Z.one 48

let to_generic m =
    Result.Res(List.map (fun (_, e) -> Result.create ~label:(pp_entry_desc e) @@ entry_to_generic e) m)

