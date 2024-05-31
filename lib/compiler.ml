let compile (source : string) =
  let scanner = Scanner.of_string source in
  let rec aux line =
    let token = Scanner.next scanner in
    if line <> token.line then Printf.printf "%4d " token.line
    else Printf.printf "   | ";
    Printf.printf "%-18s '%s'\n" (Token.show_kind token.kind) token.content;
    match token.kind with Token.Eof -> () | _ -> aux token.line
  in
  aux 0
