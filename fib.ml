#use "topfind"
#thread
#require "ketrew"

open Ketrew_pervasives ;;

let path = Printf.sprintf "/tmp/fib%d"

let debug = ref true
let fibonacci_program ?(debug = false) n =
  let p =
    if debug then
      KEDSL.Program.shf "echo computing %d; %s" n
    else
      KEDSL.Program.shf "%s"
  in
  match n with
  | 0 -> p (Printf.sprintf "echo 0 > %s" 0 (path 0))
  | 1 -> p (Printf.sprintf "echo 1 > %s" 1 (path 1))
  | n -> p (Printf.sprintf "echo $((`cat %s` + `cat %s`)) > %s"
                        (path (n - 2)) (path (n - 1)) (path (n - 1)))

module KEDSL = Ketrew.EDSL

let rec compute_fib n =
  let process = KEDSL.direct_execution (fibonacci_program n) in
  let name    = Printf.printf "fibonacci (%d)" n in
  KEDSL.file_target
    ~dependencies: [compute_fib (n - 1); compute_fib (n - 2);]
    (* Ketrew doesn't have a way of knowing that there will be multiple
       (n-2) cases (one from n and one from n-1)*)
    ~equivalence:`Same_active_condition
    (* But if we specify that targets have the same active condition. *)
    ~name
    ~make:process
    (* Then we can specify the same file target. *)
    (path n)

let unwrap = function
  | `Ok c    -> c
  | `Error _ -> failwith "unwrap"

let submit n =
  (* Create the workflow with the first argument of the command line: *)
  let workflow = compute_fib n in
  Ketrew_client.submit workflow
  |> unwrap
