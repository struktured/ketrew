- The run command in EDSL is confusing as it doesn't actually run.
  The EDSL, (for now ?) describes Targets /tasks, but doesn't have an easy command to run them.

- Running the commands is tightly integrated in the Pvem error monad. Use Lwt_main.run to extract
  the inner value if you don't want to chain.
