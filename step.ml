
#use "topfind"
#thread
#require "ketrew"

let () = 
  let open Ketrew_pervasives in
  let module Configuration = Ketrew_configuration in 
  let cdef = Configuration.get_configuration Configuration.default_configuration_path in
  let c = Lwt_main.run cdef in
  ()
