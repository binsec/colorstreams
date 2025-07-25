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

type t =
    {
        name : string;
        ext : string;
        dir : string
    }

let reg = ref Utils.StringMap.empty

let from_file ?(create_dir = false) path =
    let exp = Str.regexp {|\(\(.*\)/\)?\([^/.]*\)\([^/]*\)$|} in
    if Str.string_match exp path 0
    then
    (
        let dir = 
            try
                Str.matched_group 2 path
            with Not_found -> "."
        in
        let name = Str.matched_group 3 path in
        let ext = Str.matched_group 4 path in
        if create_dir then ignore @@ Sys.command ("mkdir -p " ^ dir);
        {dir; name; ext}
    )
    else raise (Failure ("invalid file path <" ^ path ^ ">"))

let dir = lazy
    (
        let date = Unix.localtime @@ Unix.time () in
        let target = from_file @@ Options.get_target () in
        let name = Printf.sprintf "/tmp/colorstreams-%s-%d-%d-%d-%d:%d:%d" target.name date.Unix.tm_year date.tm_mon date.tm_mday date.tm_hour date.tm_min date.tm_sec in
        ignore @@ Sys.command ("mkdir -p " ^ name);
        name
    )

let keep_temp = ref "no"

let create ?(temporary = true) ?(keep = false) name ext =
    let name =
        let cnt =
            try
                let cnt = Utils.StringMap.find name !reg in
                cnt := !cnt + 1;
                !cnt
            with Not_found ->
            (
                reg := Utils.StringMap.add name (ref 0) !reg;
                0
            )
        in
        let name = String.map (fun c -> if c = ' ' then '_' else c) name in
        if cnt = 0
        then name
        else Printf.sprintf "%s_%d" name cnt
    in
    let dir =
        if temporary
        then 
        (
            if (not keep) || !keep_temp = "no"
            then Lazy.force dir
            else 
            (
                ignore @@ Sys.command ("mkdir -p " ^ !keep_temp);
                !keep_temp
            )
        )
        else "."
    in
    let ext =
        if not (String.starts_with ~prefix:"." ext)
        then "." ^ ext
        else ext
    in
    {name; ext; dir}

let get_dir f =
    f.dir

let get_name f =
    f.name

let get_ext f =
    f.ext

let get_path f =
    Printf.sprintf "%s/%s%s" f.dir f.name f.ext

let delete f =
    ignore @@ Sys.command ("rm -f " ^ (get_path f))

let copy f1 f2 =
    ignore @@ Sys.command ("cp " ^ (get_path f1) ^ " " ^ (get_path f2))

let read f =
    try
        let chan = open_in @@ get_path f in
        let lines = ref [] in
        let res =
            try
                while true;
                do
                    lines := (input_line chan)::!lines
                done;
                !lines
            with End_of_file -> !lines
        in
        close_in chan;
        List.rev res
    with e -> raise (Failure(Printf.sprintf "Could not read file <%s> (%s)." (get_path f) @@ Printexc.to_string e))

let write s f =
    try
        let chan = open_out @@ get_path f in
        Printf.fprintf chan "%s" s;
        close_out chan
    with e -> raise (Failure(Printf.sprintf "Could not write to file <%s> (%s)." (get_path f) @@ Printexc.to_string e))

let write_lines l f =
    try
        let chan = open_out @@ get_path f in
        List.iter (fun l -> Printf.fprintf chan "%s\n" l) l;
        close_out chan
    with e -> raise (Failure(Printf.sprintf "Could not write to file <%s> (%s)." (get_path f) @@ Printexc.to_string e))

let open_out_chan f =
    open_out @@ get_path f

let open_in_chan f =
    open_in @@ get_path f

let _ =
    Options.register_option ("-keep-tmp", Arg.Set_string keep_temp, "Keep temporary files in specified directory.")
