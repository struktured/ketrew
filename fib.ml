#use "topfind"
#thread

open Ketrew_pervasives
module KEDSL = Ketrew.EDSL

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
  | 0 -> p (Printf.sprintf "echo 0 > %s" (path 0))
  | 1 -> p (Printf.sprintf "echo 1 > %s" (path 1))
  | n -> p (Printf.sprintf "echo $((`cat %s` + `cat %s`)) > %s" (path (n - 2)) (path (n - 1)) (path n))


let rec fib_depencencies = function
  | 0 -> []
  | 1 -> []
  | n -> [compute_fib (n - 1); compute_fib (n - 2)]
and compute_fib n =
  let process = KEDSL.daemonize ~starting_timeout:30.0 ~using:`Python_daemon (fibonacci_program n) in
  let name    = Printf.sprintf "fibonacci (%d)" n in
  KEDSL.file_target
    ~dependencies: (fib_depencencies n)
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
