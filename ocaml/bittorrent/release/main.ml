open Async.Std
open Metainfo

let _ =
  if Array.length Sys.argv <> 2 then begin
    printf "Wrong number of arguments.  Usage: cs3110 run main <torrent file>\n";
    exit 1
  end else

  parse_torrent_file Sys.argv.(1) >>= function
    | `Failed    -> printf "Error: Invalid torrent file\n"; exit 1
    | `Success m ->

  Session.create m >>= fun s ->

  printf "output\n";
  UploadManager.upload s;
  DownloadManager.download s


let _ = Scheduler.go ()

