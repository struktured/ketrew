#use "topfind"
#thread
#require "ketrew"

open Ketrew_pervasives ;;
module Configuration = Ketrew_configuration ;;

let run_local_shell cmd =
  let module KEDSL = Ketrew.EDSL in
  let program = KEDSL.Program.sh cmd in
  let process = KEDSL.direct_execution program in
  KEDSL.target "run_command_with_lsf" ~make:process


let unwrap = function
  | `Ok c    -> c
  | `Error _ -> failwith "unwrap"

let run_first_arg () =
  (* Create the  workflow with the first argument of the command line: *)
  let workflow = run_local_shell Sys.argv.(1) in
  (* Then, `run` is the only function that “does” something, it submits the
     workflow to the engine: *)
  Ketrew.EDSL.run workflow
  (* If Ketrew is in Standalone mode, this means writing the workflow in the
     database (nothing runs yet, you need to run Ketrew's engine yourself).
     If Ketrew is in Client-Server mode, this means sending the workflow to the
     server over HTTPS. The server will start running the workflow right away.  *)

let configuration () =
    Configuration.get_configuration Configuration.default_configuration_path
    |> Lwt_main.run
    |> unwrap

let standalone () =
  let cdef = configuration () in
  match Configuration.mode cdef with
  | `Client _ -> failwith "Why Client ?"
  | `Server _ -> failwith "Why Server ?"
  | `Standalone s -> s

let step ~client =
  match Ketrew_client.get_local_engine client with
  | None -> fail (`Failure "no client")
  | Some state ->
      begin
        Ketrew_engine.step state >>=
          fun foo ->
            Printf.printf "steps: %d" (List.length foo);
            return ()
      end

let ()  =
  let () = if (Array.length Sys.argv) >= 2 then run_first_arg () in
  let c = configuration () in
  let r = Lwt_main.run (Ketrew_client.as_client ~configuration:c ~f:step) in
  unwrap r

