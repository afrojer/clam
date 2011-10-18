(*
 * File: clamsys.ml
 * Date: 2011-10-17
 *
 * PLT Fall 2011
 * CLAM Project
 * Jeremy C. Andrus <jeremya@cs.columbia.edu>
 * Robert Martin <rdm2128@columbia.edu>
 * Kevin Sun <kfs2110@columbia.edu>
 * Yongxu Zhang <yz2419@columbia.edu>
 *
 *)
open Printf

(* assume gcc is in the path *)
let gcc_path = "gcc"

(* will be something like "clamlib.o" - need to hook in auto compilation during
 * compiler build before we actually set this variable *)
let clam_extralib = ""

exception Compile_error of string

(* Execute a system command and return the output (including stderr).
 * Code shamelessly ripped from: http://rosettacode.org/wiki/Execute_a_system_command#OCaml
 *)
let syscall_check_exit_status procname stderr = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED r -> raise (Compile_error (sprintf "%s terminated with exit code (%d)\n  %s" procname r stderr))
  | Unix.WSIGNALED n -> raise (Compile_error (sprintf "%s was killed by a signal (%d)\n%!" procname n))
  | Unix.WSTOPPED n -> raise (Compile_error (sprintf "%s was stopped by a signal (%d)\n%!" procname n))
;;

let syscall ?(env=(Unix.environment ())) cmd =
  let ic, oc, ec = Unix.open_process_full cmd env in
  let buf1 = Buffer.create 96
  and buf2 = Buffer.create 48 in
  (try
     while true do Buffer.add_channel buf1 ic 1 done
   with End_of_file -> ());
  (try
     while true do Buffer.add_channel buf2 ec 1 done
   with End_of_file -> ());
  let exit_status = Unix.close_process_full (ic, oc, ec) in
  syscall_check_exit_status ("`"^cmd^"`") ("E:"^(Buffer.contents buf2)) exit_status;
  (Buffer.contents buf1,
   Buffer.contents buf2)
;;

(*
 * Wrap up gcc so we can output a binary!
 *)
let compile_c code oname =
  let fname, ochan = Filename.open_temp_file "clam-cc-" ".c" in
  let _ = Pervasives.output_string ochan code in
  let _ = Pervasives.close_out ochan in
  let _, _ = syscall (sprintf "%s -c -o %s.o %s" gcc_path fname fname) in
  let _, _ = syscall (sprintf "%s -o %s -L./ -L/usr/local/clam/lib %s.o %s"
                               gcc_path oname fname clam_extralib) in
  Sys.remove fname; Sys.remove (fname^".o");
  ()
