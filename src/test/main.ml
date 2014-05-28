open Ketrew_pervasives

module Tests = struct

  let failures = ref 0
  let add_failure () = incr failures
end

module Test_command_line = struct


  let test_1 () =
    let open Ketrew_command_line in
    let open Result in
    let cli =
      let big_flag_string_list =
        flag (option_with ~default:[] (list string)) ~name:"--option-list-string"
          ~aliases:["-O"; "--ols"]
          ~doc_value:"<OLS>" ~doc:"Some optional list string"
      in
      group "toplevel" ~short_doc:"Top-level command" 
        ~doc:"corresponds to $0" [
        command "digit" ~aliases:["one"; "two"] ~doc:"Digital stuff" (
          big_flag_string_list  @@ fun string_list ->
          flag (option int) ~name:"--num" @@ fun num ->
          (* two_args @@ fun (arg1, arg2) -> *)
          argument string ~name:"ARG1" @@ fun arg1 ->
          argument (option string) ~name:"ARG2" @@ fun arg2 ->
          render (fun () ->
              `Result (string_list, num, arg1, arg2)
            )
        );
        command "other" ~doc:"Other stuff" (
          big_flag_string_list  @@ fun string_list ->
          render (fun () ->
              `Result (string_list, None, "Bouh", None)));
      ]
    in
    let test_parse l expect =
      let res =
        try
          begin match parse ~argv:(Array.of_list l) cli with
          | `Ok (`Result s) -> `Ok s
          | `Error e -> `Error e
          end
        with e -> `Exn e
      in
      if expect res
      then ()
      else (
        Tests.add_failure ();
        let got =
          match res with
          | `Ok (some,thing,_else,than_expected) ->
            (fmt "([%s], %s, %s, %s)"
               (String.concat ~sep:", " some)
               (Option.value_map ~default:"None" ~f:(fmt "Some %d") thing)
               _else
               (Option.value ~default:"NONE" than_expected))
          | `Error e -> fmt "Error %s" (error_to_string e)
          | `Exn e -> fmt "Exn %s" (Printexc.to_string e)
        in
        Log.(sf "Parsing: [%s] → " (String.concat ~sep:"; " l) % s got @ error);
      )
    in
    let some_exn = function `Exn _ -> true | _ -> false in
    let test_parse_exn l = test_parse l some_exn in
    let test_parse_error l e =
      test_parse l (function `Error r when r = e -> true | _ -> false) in
    let test_parse_ok l o =
      test_parse l (function `Ok r when r = o -> true | _ -> false) in
    test_parse_exn [] ;
    test_parse_error ["exec"] `No_command_found;
    test_parse_error ["exec"; "digit"] (`Argument_not_found "ARG1");
    test_parse_error ["exec"; "one"] (`Argument_not_found "ARG1");
    test_parse_ok ["exec"; "one"; "A1"] ([], None, "A1", None);
    test_parse_ok ["exec"; "one"; "A1"; "A2"] ([], None, "A1", Some "A2");
    test_parse_ok ["exec"; "one"; "--num"; "42"; "A1"; "A2"]
      ([], Some 42, "A1", Some "A2");
    test_parse_error
      ["exec"; "one"; "--num"; "42"; "--num"; "43"; "A1"; "A2"]
      (`Multiple_flag_instances "--num");
    ()


  let go () =
    Log.(s "Test Ketrew_command_line" @ normal);
    test_1 ();
    ()
end

let () =
  Printf.printf "Tests of ketrew: Go!\n%!";
  Test_command_line.go ();
  match Tests.failures with
  | {contents = 0} -> exit 0
  | {contents} ->
    Log.(s "Got " % i contents % s " failures" @error);
    exit 1
