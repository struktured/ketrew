#use "topfind"
#thread
#require "ketrew"

let run_local_shell cmd =
  let module KEDSL = Ketrew.EDSL in
  let program = KEDSL.Program.sh cmd in
  let process = KEDSL.direct_execution program in
  KEDSL.target "run_command_with_lsf" ~make:process


let ()  =
  (* Create the  workflow with the first argument of the command line: *)
  let workflow = run_local_shell Sys.argv.(1) in
  (* Then, `run` is the only function that “does” something, it submits the
     workflow to the engine: *)
  let () = Ketrew.EDSL.run workflow in
  (* If Ketrew is in Standalone mode, this means writing the workflow in the
     database (nothing runs yet, you need to run Ketrew's engine yourself).
     If Ketrew is in Client-Server mode, this means sending the workflow to the
     server over HTTPS. The server will start running the workflow right away.  *)
  let open Ketrew_pervasives in
  let module Configuration = Ketrew_configuration in 
  let cdef = Configuration.get_configuration Configuration.default_configuration_path in
  let c = Lwt_main.run cdef in
  ()
