open Camlox

let run_file filename =
  let open Core in
  match Sys_unix.file_exists filename with
  | `No | `Unknown ->
      Printf.eprintf "%s: does not exist\n" filename;
      exit 127
  | `Yes -> (
      let f c = Vm.interpret (Vm.make ()) @@ In_channel.input_all c in
      match In_channel.with_file filename ~f with
      | Ok () -> ()
      | Error Err.Compile -> exit 65
      | Error Err.Runtime -> exit 70)

let repl () =
  let open Core in
  let vm = Vm.make () in
  let rec loop () =
    try
      Out_channel.(flush stdout);
      Printf.printf "> ";
      Out_channel.(flush stdout);
      (ignore @@ Vm.interpret vm @@ In_channel.(input_line_exn stdin));
      Out_channel.(flush stderr);
      loop ()
    with End_of_file -> ()
  in
  loop ()

let param =
  let open Command.Param in
  let ( let+ ) = ( >>| ) in
  let+ file = anon @@ maybe ("FILE" %: string) in
  fun () -> match file with None -> repl () | Some f -> run_file f

let () =
  Command_unix.run @@ Command.basic ~summary:"Bytecode VM for `lox`" param
