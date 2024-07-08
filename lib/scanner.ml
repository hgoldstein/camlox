type t = {
  source : string;
  mutable start : int;
  mutable current : int;
  mutable line : int;
}

let of_string source =
  { source = source ^ "\x00"; start = 0; current = 0; line = 1 }

let current_slice scanner =
  String.sub scanner.source scanner.start (scanner.current - scanner.start)

let make_token scanner kind =
  Token.{ kind; line = scanner.line; content = current_slice scanner }

let error_token (scanner : t) content =
  Token.{ kind = Error; line = scanner.line; content }

let advance scanner =
  scanner.current <- scanner.current + 1;
  scanner.source.[scanner.current - 1]

let skip_one scanner = ignore @@ advance scanner
let peek s = s.source.[s.current]

let rec skip_while s f =
  if f (peek s) then (
    skip_one s;
    skip_while s f)
  else ()

(* NOTE: clox does this based off of a null terminated string, could be worth
 * revisiting in the future, but it makes a nice property that nothing matches
 * the null terminated string.
 *)
let is_at_end scanner = peek scanner = '\x00'
let peek_next s = if is_at_end s then None else Some s.source.[s.current + 1]

let peek_expect scanner expected =
  if is_at_end scanner then false
  else if peek scanner <> expected then false
  else (
    scanner.current <- scanner.current + 1;
    true)

let rec skip_to_line_end s =
  if is_at_end s then ()
  else
    match peek s with
    | '\n' -> ()
    | _ ->
        skip_one s;
        skip_to_line_end s

let rec skip_whitespace s =
  match peek s with
  | ' ' | '\r' | '\t' ->
      skip_one s;
      skip_whitespace s
  | '\n' ->
      s.line <- s.line + 1;
      skip_one s;
      skip_whitespace s
  | '/' -> (
      match peek_next s with
      | Some '/' ->
          skip_to_line_end s;
          skip_whitespace s
      | Some _ | None -> ())
  | _ -> ()

let rec string_ s =
  if is_at_end s then error_token s "Unterminated string."
  else
    match peek s with
    | '\n' ->
        s.line <- s.line + 1;
        skip_one s;
        string_ s
    | '"' ->
        skip_one s;
        make_token s Token.String
    | _ ->
        skip_one s;
        string_ s

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let number s =
  skip_while s is_digit;
  (match (peek s, peek_next s) with
  | '.', Some i when is_digit i ->
      skip_one s;
      skip_while s is_digit
  | _ -> ());
  make_token s Token.Number

let identifier s =
  skip_while s (fun c -> is_digit c || is_alpha c);
  let kind =
    let open Token in
    match current_slice s with
    | "and" -> And
    | "class" -> Class
    | "else" -> Else
    | "false" -> False
    | "for" -> For
    | "fun" -> Fun
    | "if" -> If
    | "nil" -> Nil
    | "or" -> Or
    | "print" -> Print
    | "return" -> Return
    | "super" -> Super
    | "this" -> This
    | "true" -> True
    | "var" -> Var
    | "while" -> While
    | _ -> Identifier
  in
  make_token s kind

let next s =
  skip_whitespace s;
  s.start <- s.current;
  if is_at_end s then make_token s Token.Eof
  else
    match advance s with
    | '(' -> make_token s Token.LeftParen
    | ')' -> make_token s Token.RightParen
    | '{' -> make_token s Token.LeftBrace
    | '}' -> make_token s Token.RightBrace
    | ';' -> make_token s Token.Semicolon
    | ',' -> make_token s Token.Comma
    | '.' -> make_token s Token.Dot
    | '-' -> make_token s Token.Minus
    | '+' -> make_token s Token.Plus
    | '/' -> make_token s Token.Slash
    | '*' -> make_token s Token.Star
    | '!' when peek_expect s '=' -> make_token s Token.BangEqual
    | '!' -> make_token s Token.Bang
    | '=' when peek_expect s '=' -> make_token s Token.EqualEqual
    | '=' -> make_token s Token.Equal
    | '<' when peek_expect s '=' -> make_token s Token.LessEqual
    | '<' -> make_token s Token.Less
    | '>' when peek_expect s '=' -> make_token s Token.GreaterEqual
    | '>' -> make_token s Token.Greater
    | '"' -> string_ s
    | c when is_digit c -> number s
    | c when is_alpha c -> identifier s
    | _ -> error_token s "Unexpected character."
