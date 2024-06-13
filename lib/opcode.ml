type t =
  | Return
  | Constant
  | Negate
  | Add
  | Subtract
  | Multiply
  | Divide
  | Nil
  | True
  | False
  | Not
  | Equal
  | Greater
  | Less
  | Print
  | Pop
  | DefineGlobal
  | GetGlobal
  | SetGlobal
  | GetLocal
  | SetLocal
  | JumpIfFalse
  | Jump
  | Loop

(* NOTE: I don't love using Obj.magic here but it's the easiest way to
 * have this converstion function w/o codegen or writing things by hand.
 *)
let to_byte (op : t) : char =
  let c = Obj.magic op in
  let i = Char.code c in
  assert (i >= 0 && i <= 255);
  c

let of_byte : char -> t = Obj.magic
