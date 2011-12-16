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

open Ast
open Vast

type equation_side = LValue | RValue

(* Retrieve type of an identifier *)
let env_type_of_ident ref_env s =
  let env = !ref_env in
    let matches idT = (s = idT.id) in
      try
        let id = List.find matches env.ids in
          id.typ
      with Not_found -> raise(Failure("Undefined Identifier: " ^ s))
 
  
(* Declare a variable name. *)
(* Add it to our list of identifiers, and if it is an image, add it to the list of images *)
let env_declare ref_env ident_string typ =
  let matches = fun idT -> (idT.id = ident_string) in
    let env = !ref_env in
      if (List.exists matches env.ids) then
        (raise(Failure("Identifier " ^ ident_string ^ " was declared twice.")))
      else
        let new_ids =
          ({ id = ident_string; typ = typ; init = false; chans = []; } :: env.ids)
        in
        ref_env.contents <- { ids = new_ids; }
      

(* Helper function: look for an identifier and set "initialized=true" if it's an lvalue *)
let do_env_exists ref_env ident_string typ eq_side =
  let env = !ref_env in
    let fold_ids (ids, is_found) next_id =
      if (next_id.id = ident_string) then
        if (next_id.typ = typ) then
          let next_id = 
            let new_init = next_id.init || match eq_side with LValue -> true | RValue -> false in
               {
                 id = next_id.id;
                 typ = typ;
                 init = new_init;
                 chans = next_id.chans;
               }
          in
          (next_id :: ids, true)
        else
          raise(Failure("Assigning identifier " ^ ident_string ^ " (" ^ (Printer.string_of_type next_id.typ) ^ ") assigned to " ^
                        "an expression of type " ^ (Printer.string_of_type typ)))
      else
        (next_id :: ids, is_found)
    in
    let (new_ids, is_found) = List.fold_left fold_ids ([], false) env.ids in
      if not is_found then
        raise(Failure("Identifier not declared: " ^ ident_string))
      else
        ref_env.contents <- { ids = new_ids; }


(* Check Identifier for use as an L-value *)
(* If it exists, mark the assigned variable as initialized *)
let env_assign ref_env ident_string typ =
  do_env_exists ref_env ident_string typ LValue
 
(* Check Identifier for use as an R-value *)
let env_exists ref_env ident_string typ =
  do_env_exists ref_env ident_string typ RValue



(* Find a channel in an image and acts based on which side of the equation *)
let find_channel ref_env ch eq_side =
  let env = !ref_env in
    let fold_ids (ids, is_found) next_id =
      if (next_id.id = ch.image) then
        if (next_id.typ = ImageType) then
          let next_id = 
            match eq_side with
                LValue ->
                   {
                     id = next_id.id;
                     typ = ImageType;
                     init = true;
                     chans = ch.channel :: next_id.chans;
                   }
              | RValue -> next_id
          in
          (next_id :: ids, true)
        else
          raise(Failure("Only Image types have channels. '" ^ ch.image ^ "' is of type: " ^ (Printer.string_of_type next_id.typ)))
      else
        (next_id :: ids, is_found)
    in
    let (new_ids, is_found) = List.fold_left fold_ids ([], false) env.ids in
      if not is_found then
        raise(Failure("Could not find the image associated with the ChanRef: " ^ (Printer.string_of_chan ch)))
      else
        ref_env.contents <- { ids = new_ids; }
 
  

(* Check ChanRef for use as L-value *)
let env_assign_chan ref_env ch =
  find_channel ref_env ch LValue

(* Check ChanRef for use as R-value *)
let env_exists_chan ref_env ch =
  find_channel ref_env ch RValue



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
