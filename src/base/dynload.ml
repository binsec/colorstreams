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

let load_script file =
    let fname = File.get_name file in
    let src = File.create fname "ml" in
    File.copy file src;
    let res =
        if Dynlink.is_native
        then
        (
            let target = File.create fname "cmxs" in
            ignore @@ Sys.command @@ Printf.sprintf "ocamlfind ocamlopt -o %s -thread -shared -package colorstreams %s" (File.get_path target) @@ File.get_path src;
            target
        )
        else
        (
            ignore @@ Sys.command @@ Printf.sprintf "ocamlfind ocamlc -package colorstreams -c %s" @@ File.get_path src;
            File.create fname "cmo"
        )
    in
    Dynlink.loadfile @@ File.get_path res

let load_plugin file =
    if Dynlink.is_native
    then Dynlink.loadfile @@ File.get_path file
    else raise (Failure "cannot load .cmxs in bytecode")

let load_bytecode file =
    if Dynlink.is_native
    then raise (Failure "cannot load bytecode in native code")
    else Dynlink.loadfile @@ File.get_path file

let load file =
    let file = File.from_file file in
    match File.get_ext file with
    | ".ml" -> load_script file
    | ".cmxs" -> load_plugin file
    (*| ".cmo"
    | ".cma" -> load_bytecode file*)
    | _ -> raise (Failure "unsupported file type")

let _ =
    Options.register_option ("-load", Arg.String load, "Load a plugin (.cmxs) or a script (.ml).")
