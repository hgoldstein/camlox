# `camlox`

It's a `clox` implementation in OCaml, see: https://craftinginterpreters.com/. This implementation does pass the included test suite, though it's performance leaves some things to be desired.

## Notes

Obviously: I didn't have to deal with garbage collection (sorry).

I wanted to write idiomatic OCaml where I could while also following the rough architecture of the book, so there are some weird compromises. First, I would not attempt to do any real performance analysis without [`flambda`](https://ocaml.org/manual/5.2/flambda.html) 

I use a list rather than an array to handle the VM stack, which did hurt performance, though in practice it was a small amount of time spent even on function call heavy benchmarks. What it did allow me to do was use pattern matching to handle runtime type checks. However I probably pay some performance costs because of what _should_ be unrepresentable state. For example:
```
var a = 1;
var b = 2;
return a + b;
```
When we compile this snippet, we _know_ that there are two items on the stack. `clox` will pop them and then do the runtime typechecks. We _must_ have some sort of catch here, even if it was some sort of wildcard `failwith "UH OH"` pattern.

Something that mildly surprised me is that OCaml performance with `mutable` record fields is ... quite bad. While I didn't see such for the instruction pointer field, every other time I made the VM _more_ immutable (removed a `mutable` field), I saw a significant performance win.

There are two things I'd want to experiment with as a general `lox` implementation rather than one that is trying to follow the book (within reason).
- I'm curious how much having an array backed stack matters for performance.
- Reading instructions is quite expensive: we pay a lot more for pointer calculating / dereferencing in OCaml (based on me staring a bit at assembly output from Godbolt). Fat instructions could help smooth that over and make things a little more readable. We're in OCamlland anyhow.