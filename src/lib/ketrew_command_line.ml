open Ketrew_pervasives

type _ argument_type =
  | Option: 'a argument_type -> ('a option) argument_type
  | Option_with_default: 'a argument_type * 'a -> 'a argument_type
  | String: string argument_type
  | Int: int argument_type
  | List: 'a argument_type * string -> ('a list) argument_type
  (* lists have a separator *)
  | Bool: bool argument_type
  
let option o = Option o
let option_with ~default o = Option_with_default (o, default)
let string = String
let int = Int
let list ?(sep=",") s = List (s, sep)

type 'a flag = {
  flag_name: string;
  flag_aliases: string list;
  flag_doc_value: string option;
  flag_doc_string: string option;
  flag_type: 'a argument_type;
}
type 'a argument = {
  argument_doc_name: string;
  argument_doc_string: string option;
  argument_type: 'a argument_type;
}
type _ arg_list =
  | End_args: (unit -> 't) -> 't arg_list
  | Add_flag: 'a flag * ('a -> 'b arg_list) -> 'b arg_list
  | Add_argument: 'a argument * ('a -> 'b arg_list) -> 'b arg_list

let flag t ?(aliases=[]) ?doc_value ?doc ~name cont =
  Add_flag ({ flag_name = name; flag_aliases = aliases;
              flag_doc_value = doc_value; flag_doc_string = doc;
              flag_type = t; }, cont)
let argument t ?doc_value ?doc ~name cont =
  Add_argument ({ argument_doc_name = name;
                  argument_doc_string = doc;
                  argument_type = t; }, cont)

let render v = End_args v

(*
type 'a arguments = {
  command_name: string;
  command_aliases: string list;
  command_arguments: 'a arg_list;
  command_short_doc: string;
  command_doc: string;
}
*)
type command = {
  command_name : string;
  command_aliases : string list;
  command_doc: string;
  command_short_doc: string;
}
type 'a command_line_spec = 
  | Command of (command * 'a arg_list)
  | Group of command * 'a command_line_spec list

let command ?(aliases=[]) ?(short_doc="") ?(doc="") name args =
  Command ({ command_name = name; command_aliases = aliases;
             command_short_doc = short_doc; command_doc = doc }, args)
let group ?(aliases=[]) ?(short_doc="") ?(doc="") name l =
  Group ({ command_name = name; command_aliases = aliases;
           command_short_doc = short_doc; command_doc = doc }, l)
  (* Group ({group_name = name; group_doc = doc; group_aliases = aliases}, l) *)

open Result
let parse_command arg_list spec =
  let rec argument_witness: type a. a argument_type -> a = 
    function
    | Option _ -> None
    | Bool -> false
    | Int -> 42
    | List _ -> []
    | Option_with_default (_, d) -> d
    | String -> ""
  in
  let rec argument_kind: type a. a argument_type -> _ = 
    function
    | Option t -> argument_kind t
    | List (t, _) -> argument_kind t
    | Option_with_default (t, _) -> argument_kind t
    | Bool -> `Single
    | other -> `With_argument
  in
  (*
  let argument_cardinality: type a. a argument_type -> _ = 
    function
    | Option t -> `Optional
    | List (t, _) -> `List
    | Option_with_default (t, _) -> `Optional
    | other -> `Required
  in
  *)
  let rec find_flag: type a. a arg_list -> string -> _  option =
    fun spec n ->
      match spec with
    | End_args _ -> None
    | Add_flag (flg, continue) -> 
      if flg.flag_name = n || List.mem ~set:flg.flag_aliases n
      then Some (argument_kind flg.flag_type)
      else find_flag (continue (argument_witness flg.flag_type)) n
    | Add_argument (arg, continue) ->
      find_flag (continue (argument_witness arg.argument_type)) n
  in
      (* let kind = argument_kind flg.flag_type in *)
  let preparse_exn ~unknown_flag ~missing_argument argl spec =
    let rec preparse = function
    | [] -> []
    | "--" :: rest -> List.map rest (fun r -> `Argument r)
    | h :: t when String.get h 0 = Some '-' ->
      begin match find_flag spec h with
      | None -> unknown_flag h
      | Some `Single -> `Single_flag h :: preparse t
      | Some `With_argument ->
        begin match t with
        | one_more :: rest ->
          `Flag_with_argument (h, one_more) :: preparse rest
        | [] -> missing_argument h
        end
      end
    | h :: t -> `Argument h :: preparse t
    in
    preparse argl
  in

  let rec parse_value: type a. string -> a argument_type -> (a, _) Result.t =
    fun v -> function
    | Option t -> parse_value v t >>= fun o -> return (Some o)
    | List (t, sep) -> failwithf "todo: List"
    | String -> return v
    | Int -> 
      begin match Int.of_string v with 
      | None -> fail (`Parsing_error (`Int, v))
      | Some s -> return s
      end
    | Bool -> assert false
    | Option_with_default (t, _) -> parse_value v t
  in
  let rec g preparsed =
    function
    | End_args v -> 
      begin match preparsed with
      | [] -> return (v ())
      | some -> fail (`Unexpected_arguments "TODO")
      end
    | Add_flag (flag, continue) ->
      let set = flag.flag_aliases in
      let matches n =  flag.flag_name = n || List.mem ~set n  in
      let rec refind_flag acc_yes acc_no = function
      | [] -> (List.rev acc_yes, List.rev acc_no)
      | (`Argument _ as a) :: more  -> refind_flag acc_yes (a :: acc_no) more
      | (`Single_flag n as s) :: more  when matches n ->
        refind_flag (s :: acc_yes) acc_no more
      | (`Flag_with_argument (f, a) as s) :: more  when matches f ->
        refind_flag (s :: acc_yes) acc_no more
      | other  :: more -> refind_flag acc_yes (other :: acc_no) more
      in
      begin match refind_flag [] [] preparsed, flag.flag_type with
      | ([], rest), Option _ -> continue None |> g rest
      | ([], rest), Option_with_default (_, v) -> continue v |> g rest
      | ([], rest), List _ -> continue [] |> g rest
      | ([], rest), Bool -> continue false |> g rest
      | ([], rest), other -> fail (`Flag_not_found flag.flag_name)
      | ([`Single_flag _], rest), Bool -> continue true |> g rest
      | ([`Flag_with_argument (_, v)], rest), t ->
        parse_value v t
        >>= fun value ->
        continue value |> g rest
      | some_list, List (t, _) -> failwith "Lists not implemented"
      | other_stuff, t -> fail (`Multiple_flag_instances flag.flag_name)
      end
    | Add_argument (arg, f) ->
      let rec next_argument = function
      | [] -> (None, [])
      | `Argument a :: more -> (Some a, more)
      | nonarg :: more ->
        let (res, remain) = next_argument more in
        (res, nonarg :: remain)
      in
      let arg_opt, rest = next_argument preparsed in
      begin match arg_opt, arg.argument_type with
      | None, Option _ -> f None |> g rest
      | None, List _ -> f [] |> g rest
      | None, Option_with_default (_, v) -> f v |> g rest
      | None, other -> fail (`Argument_not_found arg.argument_doc_name)
      | Some a, t -> 
        parse_value a t >>= fun value -> f value |> g rest
      end
  in
  let unknown_flag f = raise (Failure f) in 
  let missing_argument s = raise (Invalid_argument s) in
  begin try
    return (preparse_exn ~unknown_flag ~missing_argument arg_list spec)
  with Failure f -> fail (`Unknown_flag f)
     | Invalid_argument s -> fail (`Missing_argument s)
  end
  >>= fun preparsed ->
  g preparsed spec

let parse ?(argv=Sys.argv) spec =
  let arg_list = Array.to_list argv in
  let rec find_subcommand args spec =
    List.find_map spec ~f:(function
      | Group ({command_name; command_aliases; _}, cmds) ->
        begin match args with
        | [] -> None
        | h :: t when h = command_name || List.mem h command_aliases ->
          find_subcommand t cmds
        | h :: t -> None
        end
      | Command ({command_name; command_aliases; _}, argspec) ->
        begin match args with
        | [] -> None
        | h :: t when h = command_name || List.mem h command_aliases ->
          Some (t, argspec)
        | h :: t -> None
        end)
  in
  match spec with
  | Command (_, cmd) ->
    parse_command (List.tl_exn arg_list) cmd 
  | Group (_, commands) ->
    begin match find_subcommand (List.tl_exn arg_list) commands with
    | Some (args, found) -> parse_command args found
    | None -> fail (`No_command_found)
    end

let error_to_string = function
| `No_command_found -> "No command found"
| `Flag_not_found f -> fmt "Mandatory flag %S not found" f
| `Argument_not_found a -> fmt "Argument %S not found" a
| `Missing_argument f -> fmt "Missing argument for flag %S" f
| `Parsing_error (`Int, v) -> fmt "Parsing error: expecting int but got %S" v
| `Unexpected_arguments s -> fmt "Unexpected arguments"
| `Unknown_flag f -> fmt "Unknown flag %S" f
| `Multiple_flag_instances f -> fmt "Flag %S given too many times" f



