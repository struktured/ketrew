open Ketrew_pervasives;;
open Ketrew;;
(*open EDSL;; *)

let c = Configuration.engine () ;;
let run = Engine.with_engine ~configuration:c  ;;
let print_number_of_targets ~engine = Engine.current_targets engine >>| List.length >>| Printf.printf "%d\n" ;;
let take_a_step ~engine = Engine.with_engine ~configuration:c (fun ~engine -> Engine.step engine >>| ignore) ;;
let _ = run print_number_of_targets  ;;
(*let t = target ~make:(direct_shell_command "date > /tmp/hello_world") "" ;; *)
(* let r = run t *)
