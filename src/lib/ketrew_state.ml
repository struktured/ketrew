open Ketrew_pervasives
open Ketrew_long_running

module Path = Ketrew_path
module Host = Ketrew_host
module Artifact = Ketrew_artifact
module Target = Ketrew_target
module Database = Ketrew_database

module Nohup_setsid = Ketrew_nohup_setsid

module Persistent_state = struct
  type t = Ketrew_gen_base_v0_t.persistent_state = {
    current_targets: Target.id list;
    (* keep db id of a list of all "archived" targets *)
  }
  let create () = {current_targets = [];}

  let serialize t =
    Ketrew_gen_versioned_j.string_of_persistent_state (`V0 t)

  let deserialize s = 
    try return (
        match Ketrew_gen_versioned_j.persistent_state_of_string s with
        | `V0 v0 -> v0
      )
    with e -> fail (`Persistent_state (`Deserilization (Printexc.to_string e)))

  let add t target = { current_targets = Target.id target :: t.current_targets }

  let current_targets t = t.current_targets
end

module Configuration = struct

  type t = {
    database_parameters: string;
    persistent_state_key: string;
    turn_unix_ssh_failure_into_target_failure: bool;
  }

  let default_persistent_state_key = "ketrew_persistent_state"

  let default_configuration_path = 
    Sys.getenv "HOME" ^ "/.ketrew/client.toml"

  let default_database_path = 
    Sys.getenv "HOME" ^ "/.ketrew/database.json"


  let create 
      ?(turn_unix_ssh_failure_into_target_failure=false)
      ?(persistent_state_key=default_persistent_state_key) ~database_parameters () =
    {
      database_parameters; persistent_state_key;
      turn_unix_ssh_failure_into_target_failure;
    }

  let parse_exn str =
    let toml = Toml.from_string str in
    let turn_unix_ssh_failure_into_target_failure =
      try 
        Toml.get_bool toml "unix-ssh-make-targets-fail"
        || Toml.get_bool toml "turn-unix-ssh-failure-into-target-failure"
      with
      | _ -> false
    in
    let table =
      try Toml.get_table toml "database" with
      | Not_found -> failwith "Configuration file without [database]" in
    let database_parameters = 
      try Toml.get_string table "path" with
      | _ -> failwith "missing database path" in
    let persistent_state_key =
      try Toml.get_string table "state-key" with 
      | _ -> default_persistent_state_key in
    create ~turn_unix_ssh_failure_into_target_failure
      ~persistent_state_key ~database_parameters ()

  let parse s =
    let open Result in
    try parse_exn s |> return 
    with e -> fail (`Configuration (`Parsing (Printexc.to_string e)))


  let get_configuration ?override_configuration path =
    match override_configuration with
    | Some c -> return c
    | None ->
      IO.read_file path
      >>= fun content ->
      of_result (parse content)
end


type t = {
  mutable database_handle: Database.t option;
  configuration: Configuration.t;
  long_running_plugins: (string * (module LONG_RUNNING)) list;
}
let default_plugins = [
  Nohup_setsid.name, (module Nohup_setsid: LONG_RUNNING);
  Ketrew_lsf.name, (module Ketrew_lsf: LONG_RUNNING);
]
let create ?(plugins=default_plugins) configuration =
  return {database_handle = None; configuration;
          long_running_plugins = plugins}

let not_implemented msg = 
  Log.(s "Going through not implemented stuff: " % s msg @ verbose);
  fail (`Not_implemented msg)

let database t =
  match t.database_handle with
  | Some db -> return db
  | None -> 
    let path = t.configuration.Configuration.database_parameters in
    Database.load path
    >>= fun db ->
    t.database_handle <- Some db;
    return db

let get_persistent t =
  database t >>= fun db ->
  begin Database.get db ~key:t.configuration.Configuration.persistent_state_key
    >>= function
    | Some persistent_serialized ->
      Persistent_state.deserialize persistent_serialized
    | None ->
      let e = Persistent_state.create () in
      return e
  end

let save_persistent t persistent =
  database t >>= fun db ->
  let key = t.configuration.Configuration.persistent_state_key in
  let action = Database.(set ~key (Persistent_state.serialize persistent)) in
  begin Database.act db ~action
    >>= function
    | `Done -> return ()
    | `Not_done -> fail (`Database_unavailable key)
  end

let add_or_update_target t target =
  database t
  >>= fun db ->
  begin Database.(act db (set target.Target.id Target.(serialize target)))
    >>= function
    | `Done -> return ()
    | `Not_done ->
      (* TODO: try again a few times instead of error *)
      fail (`Database_unavailable target.Target.id)
  end

let add_target t target =
  add_or_update_target t target
  >>= fun () ->
  get_persistent t
  >>= fun persistent ->
  let new_persistent = Persistent_state.add persistent target in
  save_persistent t new_persistent
(* TODO: remove target if this fails, or put in same transaction *)

let get_target db id =
  Database.get db id
  >>= function
  | Some t -> of_result (Target.deserialize t)
  | None -> fail (`Missing_data id)

let current_targets t =
  database t >>= fun db ->
  get_persistent t >>= fun persistent ->
  let target_ids = Persistent_state.current_targets persistent in
  Deferred_list.for_concurrent target_ids ~f:(get_target db)
  >>= fun (targets, errors) ->
  begin match errors with
  | [] -> return targets
  | some :: more -> fail some (* TODO do not forget other errors *)
  end

let _check_and_activate_dependencies ~t ids =
  database t >>= fun db ->
  let what_happened = ref [] in
  let happened h = what_happened := h :: !what_happened in
  Deferred_list.while_sequential ids ~f:(fun dep ->
      get_target db dep >>< function
      | `Ok dependency ->
        begin match dependency.Target.history with
        | `Created _  ->
          let newdep = Target.(activate_exn dependency ~by:`Dependency) in
          add_or_update_target t newdep
          >>= fun () ->
          `Target_activated (Target.id dependency, `Dependency) |> happened;
          return `Wait
        | `Activated _ | `Running _ -> return `Wait
        | `Dead _ -> return (`Die dep)
        | `Successful _ -> return `Go
        end
      | `Error (`Missing_data s) ->
        (* Dependency not-found => should get out of the way *)
        return (`Die dep)
      | `Error (`Target _ as e) -> fail e
    )
  >>= fun statuses ->
  let happenings = List.rev !what_happened in 
  begin match statuses with
  | some_list when List.for_all some_list ~f:((=) `Go) ->
    return (`Go_now, happenings)
  | some_dependency_died
    when List.exists some_dependency_died
        ~f:(function `Die _ -> true | _ -> false) ->
    return (`Some_dependencies_died
              (List.filter_map some_dependency_died
                 ~f:(function `Die d -> Some d | _ -> None)),
            happenings)
  | no_death_but_not_all_go -> return (`Wait, happenings)
  end

let with_plugin_or_kill_target t ~target ~plugin_name f =
  begin match 
    List.find t.long_running_plugins (fun (n, _) -> n = plugin_name)
  with
  | Some (_, m) ->
    f m
  | None -> 
    add_or_update_target t Target.(
        make_fail_exn target  
          ~msg:(fmt "Plugin not found: %s" plugin_name))
    >>= fun () ->
    return [`Target_died (Target.id target, `Plugin_not_found plugin_name)]
  end

let _start_running_target t target =
  begin match target.Target.make with
  | `Artifact a ->
    add_or_update_target t Target.(make_succeed_exn target a)
    >>= fun () ->
    return [`Target_succeeded (Target.id target, `Artifact_literal)]
  | `Get_output cmd ->
    begin Target.Command.get_output cmd
      >>< function
      | `Ok (out, _) ->
        Log.(s "Cmd output: " % s out @ very_verbose);
        let new_target =
          Target.make_succeed_exn target (`Value (`String out)) in
        add_or_update_target t new_target
        >>= fun () ->
        return [`Target_succeeded (Target.id target, `Process_success)]
      | `Error (`Host e) ->
        begin match Host.Error.classify e with
        | `Ssh | `Unix ->
          Log.(s "SSH failed, but not killing " % s (Target.id target)
               % sp % Host.Error.log e @ warning);
          return []
        | `Execution ->
          add_or_update_target t Target.(
              make_fail_exn target  
                ~msg:(fmt "Host error: %s" 
                        (Host.Error.log e |> Log.to_long_string)))
          >>= fun () ->
          return [`Target_died (Target.id target, `Process_failure)]
        end
    end
  | `Direct_command cmd ->
    begin Target.Command.run cmd
      >>< function
      | `Ok () -> 
        begin Artifact.is_ready target.Target.result_type
          >>= function
          | false when target.Target.result_type <> `Value `Unit ->
            add_or_update_target t Target.(
                make_fail_exn target  
                  ~msg:(fmt "command %S did not create %S" 
                          (Command.to_string_hum cmd)
                          (Artifact.Type.to_string_hum target.result_type)))
            >>= fun () ->
            return [`Target_died (Target.id target, `Process_failure)]
          | _ ->
            (* result_type must be a Volume: *)
            begin match Artifact.of_type target.Target.result_type with
            | Some v ->
              add_or_update_target t Target.(make_succeed_exn target v)
              >>= fun () ->
              return [`Target_succeeded (Target.id target, `Process_success)]
            | None ->
              add_or_update_target t Target.(
                  make_fail_exn target  
                    ~msg:(fmt "wrong typing of the target: \
                               direct-command cannot produce %s" 
                            (Artifact.Type.to_string_hum target.result_type)))
              >>= fun () ->
              return [`Target_died (Target.id target, `Wrong_type)]
            end
        end
      | `Error (`Host e) ->
        add_or_update_target t Target.(
            make_fail_exn target  
              ~msg:(fmt "host: %s" (Host.Error.log e |> Log.to_long_string)))
        >>= fun () ->
        return [`Target_died (Target.id target, `Process_failure)]
    end
  | `Long_running (plugin_name, created_run_paramters) ->
    with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
        let module Long_running = (val m : LONG_RUNNING) in
        let c = Long_running.deserialize_exn created_run_paramters in
        begin Long_running.start c
          >>< function
          | `Ok run_parameters ->
            let run_parameters = Long_running.serialize run_parameters in
            add_or_update_target t Target.(
                set_running_exn target ~plugin_name ~run_parameters)
            >>= fun () ->
            return [`Target_started (Target.id target, plugin_name)]
          | `Error (`Failed_to_start s) ->
            add_or_update_target t Target.(
                make_fail_exn target  
                  ~msg:(fmt "[%s] %s" plugin_name s))
            >>= fun () ->
            return [`Target_died (Target.id target,
                                  `Failed_to_start (plugin_name, s))]
        end)
  end

let _update_status t ~target ~bookkeeping =
  let plugin_name = bookkeeping.Target.plugin_name in
  with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
      let module Long_running = (val m : LONG_RUNNING) in
      let run_parameters =
        Long_running.deserialize_exn bookkeeping.Target.run_parameters in
      begin Long_running.update run_parameters
        >>< function
        | `Ok (`Still_running run_parameters) ->
          let run_parameters = Long_running.serialize run_parameters in
          add_or_update_target t Target.(
              update_running_exn target ~run_parameters)
          >>= fun () ->
          return []
        | `Ok (`Succeeded run_parameters) ->
          let run_parameters = Long_running.serialize run_parameters in
          (* result_type must be a Volume: *)
          begin match Artifact.of_type target.Target.result_type with
          | Some v ->
            add_or_update_target t Target.(
                update_running_exn target ~run_parameters
                |> fun trgt ->  make_succeed_exn trgt v)
            >>= fun () ->
            return [`Target_succeeded (Target.id target, `Process_success)]
          | None ->
            add_or_update_target t Target.(
                make_fail_exn target  
                  ~msg:(fmt "wrong typing of the target: \
                             long-running cannot produce %s" 
                          (Artifact.Type.to_string_hum target.result_type)))
            >>= fun () ->
            return [`Target_died (Target.id target, `Wrong_type)]
          end
        | `Ok (`Failed (run_parameters, msg)) ->
          let run_parameters = Long_running.serialize run_parameters in
          (* result_type must be a Volume: *)
          add_or_update_target t Target.(
              update_running_exn target ~run_parameters
              |> fun trgt ->  make_fail_exn trgt ~msg
            )
          >>= fun () ->
          return [`Target_died (Target.id target, `Process_failure)]
        | `Error (`Failed_to_update s) ->
          add_or_update_target t Target.(
              make_fail_exn target  
                ~msg:(fmt "[%s] %s" plugin_name s))
          >>= fun () ->
          return [`Target_died (Target.id target,
                                `Failed_to_update (plugin_name, s))]
      end)

type happening =
  [ `Target_activated of Ketrew_target.id * [ `Dependency ]
  | `Target_died of
      Ketrew_target.id  *
      [ `Dependencies_died
      | `Failed_to_start of string * string
      | `Failed_to_update of string * string
      | `Plugin_not_found of string
      | `Wrong_type
      | `Killed
      | `Process_failure ]
  | `Target_started of Ketrew_target.id * string
  | `Target_succeeded of
      Ketrew_target.id *
      [ `Artifact_literal | `Artifact_ready | `Process_success ] ]

let log_what_happened =
  let open Log in
  function
  | `Target_activated (id, `Dependency) ->
    s "Target " % s id % s " activated: " % s "Dependency"
  | `Target_succeeded (id, how) ->
    s "Target " % s id % s " succeeded: " 
    % (match how with
      | `Artifact_ready -> s "Artifact_ready"
      | `Artifact_literal -> s "Artifact_literal"
      | `Process_success -> s "Process success")
  | `Target_started (id, plugin_name) ->
    s "Target " % s id % s " started " % parens (s plugin_name)
  | `Target_died (id, how) ->
    s "Target " % s id % s " died: " 
    % (match how with
      | `Dependencies_died -> s "Dependencies_died"
      | `Failed_to_start (plugin_name, msg) -> 
        sf "[%s] failed to start: %s" plugin_name msg
      | `Failed_to_update (plugin_name, msg) ->
        sf "[%s] failed to update: %s" plugin_name msg
      | `Plugin_not_found p -> sf "Plugin %S not found" p
      | `Killed -> s "Killed"
      | `Wrong_type ->  s "Wrong typing"
      | `Process_failure -> s "Process_failure")

let what_happened_to_string w =
  Log.to_string ~indent:0 ~line_width:max_int (log_what_happened w)

let step t =
  begin
    current_targets t >>= fun targets ->
    database t >>= fun db ->
    Deferred_list.while_sequential targets ~f:(fun target ->
        match target.Target.history with
        | `Created _ -> (* nothing to do *) return []
        | `Activated _ ->
          begin Artifact.is_ready target.Target.result_type
            >>= function
            | false ->
              _check_and_activate_dependencies ~t target.Target.dependencies
              >>= fun (what_now, happenings) ->
              begin match what_now with
              | `Go_now ->
                _start_running_target t target
              | `Some_dependencies_died l ->
                let msg = String.concat ~sep:", " l |>
                          fmt "Dependencies died: %s" in
                (* TODO also cancel other running dependencies? *)
                add_or_update_target t Target.(make_fail_exn target ~msg)
                >>= fun () ->
                return (`Target_died (Target.id target, `Dependencies_died) :: happenings)
              | `Wait -> return happenings
              end
            | true ->
              (* result_type must be a Volume or unit: *)
              begin match Artifact.of_type target.Target.result_type with
              | Some v ->
                add_or_update_target t Target.(make_succeed_exn target v)
                >>= fun () ->
                return [`Target_succeeded (Target.id target, `Artifact_ready)]
              | None ->
                add_or_update_target t Target.(
                    make_fail_exn target  
                      ~msg:(fmt "wrong typing of the target: \
                                 direct-command cannot produce %s" 
                              (Artifact.Type.to_string_hum target.result_type)))
                >>= fun () ->
                return [`Target_died (Target.id target, `Wrong_type)]
              end
          end
        (* start or run *)
        | `Running (bookkeeping, _)  ->
          _update_status t ~target ~bookkeeping
        | `Dead _ | `Successful _ -> return [])
    >>| List.concat
    >>= fun what_happened ->
    Log.(s "Step: " % OCaml.list log_what_happened what_happened 
         @ very_verbose);
    return what_happened
  end 

let get_status t id =
  database t >>= fun db ->
  get_target db id >>= fun target ->
  return target.Target.history 

let kill t ~id =
  database t >>= fun db ->
  get_target db id >>= fun target ->
  begin match target.Target.history with
  | `Created c ->
    add_or_update_target t Target.(
        kill_exn (activate_exn target ~by:`User)
          ~msg:(fmt "Manual killing"))
    >>= fun () ->
    return [`Target_died (Target.id target, `Killed)]
  | `Activated _ ->
    add_or_update_target t Target.(
        kill_exn target ~msg:(fmt "Manual killing"))
    >>= fun () ->
    return [`Target_died (Target.id target, `Killed)]
  | `Running (bookkeeping, activation) ->
    let plugin_name = bookkeeping.Target.plugin_name in
    with_plugin_or_kill_target t ~plugin_name ~target (fun m ->
        let module Long_running = (val m : LONG_RUNNING) in
        let run_parameters =
          Long_running.deserialize_exn bookkeeping.Target.run_parameters in
        Long_running.kill run_parameters
        >>= function
        | `Killed rp ->
          add_or_update_target t Target.(
              kill_exn target ~msg:(fmt "Manual killing (%s)" plugin_name))
          >>= fun () ->
          return [`Target_died (Target.id target, `Killed)])
  | `Dead _ | `Successful _ ->
    return []
  end



