open Camlox

let run_file filename =
  let open Core in
  let f c = Vm.interpret @@ In_channel.input_all c in
  match In_channel.with_file filename ~f with
  | Ok () -> ()
  | Error Err.Compile -> exit 65
  | Error Err.Runtime -> exit 70

let rec repl () =
  let open Core in
  Printf.printf "> ";
  try
    Out_channel.(flush stdout);
    (ignore @@ Vm.interpret @@ In_channel.(input_line_exn stdin));
    repl ()
  with End_of_file -> ()

let param =
  let open Command.Param in
  let ( let+ ) = ( >>| ) in
  let ( and+ ) = both in
  let+ file = anon @@ maybe ("FILE" %: string)
  and+ verbose =
    flag ~aliases:[ "v" ] "verbose" no_arg ~doc:"Turn on debug exec"
  in
  fun () ->
    if verbose then Debug.enable ();
    match file with None -> repl () | Some f -> run_file f

let () =
  Command_unix.run @@ Command.basic ~summary:"Bytecode VM for `lox`" param
