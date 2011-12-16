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

(*

(* Check a variable assignment *)
(* Mark the assigned variable as initialized *)
let check_assign env ident_string typ =
  let id =
    let has_id x = (x.id == ident_string && x.init == false) in
      try
        List.find has_id env.ids
      with Not_found -> raise(Failure("Assignment to undeclared variable: " ^ ident_string))
  in
  if
    (id.typ != typ)
  then
    raise(Failure("Assigning identifier " ^ ident_string ^ " (" ^ (Printer.string_of_type id.typ) ^ ") to an expression of type " ^ (Printer.string_of_type typ)))
  else (
    id.init <- true;
    env
  )
 
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

(* Declare a variable name. *)
(* Add it to our list of identifiers, and if it is an image, add it to the list of images *)
let declare_var ref_env ident_string typ =
  let matches = fun idT -> (idT.id == ident_string) in
    let env = !ref_env in
      if (List.exists matches env.ids) then
        (raise(Failure("Identifier " ^ ident_string ^ " was declared twice.")))
      else
        let new_ids =
          ({ id = ident_string; typ = typ; init = false; chans = []; } :: env.ids)
        in
        ref_env.contents <- { ids = new_ids; }
      


