# File `src/test/dummy_plugin.ml`

```ocaml

```

Dummy Plugin
============

This plugin “inherits” from the implementation of `Ketrew_daemonize` and adds a
new “query” (that just retrives the result of the `date` command; pretty
useless).

```ocaml
open Ketrew_pervasives

```

The name has to be “unique”; the `create` function calls
`Ketrew_daemonize.create` and changes the `name`:

```ocaml
let name = "dummy"

let create ~host program =
  let `Long_running  (_, serialized) =
    Ketrew_daemonize.create ~using:`Python_daemon ~host program in
  `Long_running (name, serialized)

```
Implementation
--------------

The module `Another_long_running` is the actual implementation of the plugin.

The functions `additional_queries` and `query` deal with the `"date"` query,
or pass the baby to the `Ketrew_daemonize` module.

```ocaml

module Another_long_running : Ketrew_long_running.LONG_RUNNING = struct
  include Ketrew_daemonize
  let name = name

  let additional_queries run_param =
    ("date", Log.(s "Display the date, not even on the right host"))
    :: additional_queries run_param

  let query run_param item =
    if item = "date" then
      begin Ketrew_host.(get_shell_command_output (of_string "/tmp")
                           (fmt "date"))
        >>< function
        | `Ok (o, _) -> return o
        | `Error e ->
          fail Log.(s "Command `date` failed: " % s (Ketrew_error.to_string e))
      end
    else (* call Ketrew_daemonize's function: *)
      query run_param item
end

```
Registration
------------
  
Registering the plugin is a simple function call using first class modules:
```ocaml
let () =
  Ketrew_plugin.register_long_running_plugin ~name (module Another_long_running)


```
