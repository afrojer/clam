(*
 * File: clamtypes.ml
 * Date: 2011-11-09
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)

open ExtString

exception ParseErr of exn * (string * int * int * string * string)

let print_clamerr = function
    ParseErr(exn,(file,line,cnum,tok,tail)) ->
      let extra = Printexc.to_string exn in
      let fname = if file = "" then "<stdin>" else file in
      let estr =
        if tok = "" then
          Printf.sprintf "%s" extra
        else
          Printf.sprintf "%s at %s:%u:%u near \"%s%s\""
            extra fname line cnum tok (String.slice ~last:32 tail)
        in
      prerr_endline estr;
  | _ -> ()

(*
type channel = {
  name : string;
  calc : string list; (* if longer than 1 element, it's a matrix calc *)
}

type imgT = {
  name : string;
  channels : string list; (* names of channels only *)
}

type kernT = {
  name : string;
  calc : channel list;
}

type env = {
  image_index  : int imgT; (* index for each declared image *)
  kernel_index : int kernT;
}

*)
