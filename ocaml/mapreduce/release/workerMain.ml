open Async

let () = Command.async_spec
    ~summary:"Run a MapReduce worker"
    ~readme:AppList.list_apps
    Command.Spec.(
      empty
      +> anon ("port" %: int)
    )
    (fun port () -> Worker.init port)
  |> Command.run

