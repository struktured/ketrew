- The `run` command in `Ketrew_edsl` is confusing as it doesn't actually run, maybe rename this to 'track'?
- The EDSL, (for now ?) describes Targets /tasks, but doesn't have an easy command to run them. Should this be a part of the EDSL or have a separate library? Maybe it makes sense to have a clearer separation between the task management part of Ketrew and the task running part.

- Running the commands is tightly integrated in the Pvem error monad. Use `Lwt_main.run` to extract
  the inner value if you don't want to chain.

- On Polymorphic variants

```
   Error: This expression has type
         (unit,
          [> `Database of Ketrew_database.error
           | `Database_unavailable of string
           | `Host of
               [> `Execution of < host : string; message : string; stderr : string option; stdout : string option >
                | `Non_zero of string * int
                | `Ssh_failure of [> `Wrong_log of string | `Wrong_status of Ketrew_unix_process.Exit_code.t ] * string
                | `System of [> `Sleep of float ] * [> `Exn of exn ]
                | `Timeout of float
                | `Unix_exec of string ]
               Ketrew_host.Error.non_zero_execution
           | `IO of [> `Read_file_exn of string * exn | `Write_file_exn of string * exn ]
           | `Missing_data of string
           | `Persistent_state of [> `Deserilization of string ]
           | `System of [> `File_info of string ] * [> `Exn of exn ]
           | `Target of [> `Deserilization of string ]
           | `Volume of [> `No_size of SmartPrint.t ] ]
          as 'a)
         t = (unit, 'a) Pvem.Result.t deferred
```
is difficult to debug, when something like this
```
   Error: This expression has type
         (unit, ExecutionError.t) Pvem.Result.t deferred
```

helps to easily pinpoint the API. 