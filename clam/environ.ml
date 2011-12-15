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
  else
    id.init <- true;
    env
 
(*
let assign_chan env img_ident_string chan_ident_string =
  let env = check_assign env img_ident_string in
    let img_has_id imgT = (imgT.iid == img_ident_string) in
      let img =
        try
          List.find img_has_id env.imgs
        with Not_found -> raise(Failure("Assignment to a channel of an undefined image: " ^ img_ident_string))
  in
  if
    (List.find ((==) chan_ident_string) img.chans)
  then
    raise(Failure("Assignment to a channel that already exists in an image: " ^ img_ident_string ^ "." ^ chan_ident_string))
  else
    img.chans <- chan_ident_string :: img.chans;
    env

(* Update the environment to know that we have assigned
 * an expression of type "typ" to identifier ident_string.
 * Throw an exception if this is bad *)
let assign_var env ident_string typ = match typ with
    VoidType -> (raise Failure("Cannot assign a void type"))
  | ChanRefType -> (raise Failure("Improperly assigning to a Channel Reference"))
  | _ -> (check_assign env ident_string typ)

(* Declare a variable name. If it is an image, add it to our list
 * of currently defined images so we can keep track of which channels
 * currently exist *)
let declare_var env ident_string typ =
  if (List.exists ((==) ident_string) env.ids) then
    (raise Failure("Identifier " ^ ident_string ^ " was declared twice."))
  else
    let new_ids =
      ({ id = ident_string; typ = typ; init = false; } :: env.ids)
    in
    let new_imgs = match typ with
          ImageType -> (({ iid = ident_string; chan = []; }) :: env.imgs)
        | _      -> env.imgs
    in
    { ids = new_ids; imgs = new_imgs; }
      
*)


