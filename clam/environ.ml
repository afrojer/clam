(*
 * File: environ.ml
 * Date: 2011-10-16
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)

open Vast

let env_type_of_ident ref_env s =
  let env = !ref_env in
    let matches idT = (s == idT.id) in
      try
        let id = List.find matches env.ids in
          id.typ
      with Not_found -> raise(Failure("Undefined Identifier: " ^ s))
 
  

let env_exists ref_env ident_string typ =
  print_endline("Cant yet check if id " ^ ident_string ^ " exists")


(* Check a variable assignment *)
(* If it exists, mark the assigned variable as initialized *)
let env_assign ref_env ident_string typ =
  let env = !ref_env in
    let fold_ids (ids, is_found) next_id =
      if (next_id.id = ident_string) then
        if (next_id.typ = typ) then
          let next_id = { id = next_id.id; typ = typ; init = true; chans = next_id.chans; } in (next_id :: ids, true)
        else
          raise(Failure("Assigning identifier " ^ ident_string ^ " (" ^ (Printer.string_of_type next_id.typ) ^ ") to an expression of type " ^ (Printer.string_of_type typ)))
      else
        (next_id :: ids, is_found)
    in
    Printer.print_env env;
    let (new_ids, is_found) = List.fold_left fold_ids ([], false) env.ids in
      if not is_found then
        raise(Failure("Assignment to undeclared variable: " ^ ident_string))
      else
        ref_env.contents <- { ids = new_ids; }
 
(*
(* Check a channel assignment *)
(* Add the channel to the image's list of channels *)
let assign_chan env img_ident chan_ident =
  let env =
    check_assign env img_ident ImageType
  in
  let ident_matches imgT = (imgT.iid == img_ident) in
    let img =
      try
        List.find ident_matches env.imgs
      with Not_found -> raise(Failure("Assignment to a channel of an undefined image: " ^ img_ident))
    in
    if
      List.find ((==) chan_ident) img.chans
    then
      raise(Failure("Assignment to a channel that already exists in an image: " ^ img_ident_string ^ "." ^ chan_ident_string))
    else (
      img.chans <- chan_ident_string :: img.chans;
      env
    )
*)

let env_exists_chan ref_env ch =
  print_endline("cant yet verify channel exists")

let env_assign_chan ref_env ch =
  print_endline("cant yet verify channel assignment")

(* Declare a variable name. *)
(* Add it to our list of identifiers, and if it is an image, add it to the list of images *)
let env_declare ref_env ident_string typ =
  let matches = fun idT -> (idT.id == ident_string) in
    let env = !ref_env in
      if (List.exists matches env.ids) then
        (raise(Failure("Identifier " ^ ident_string ^ " was declared twice.")))
      else
        let new_ids =
          ({ id = ident_string; typ = typ; init = false; chans = []; } :: env.ids)
        in
        ref_env.contents <- { ids = new_ids; }
      


