# NOTES

- On making a custom OCaml compiler: https://ocaml.org/releases/5.1.1#configuration-options
> For instance, one can install a switch with both flambda and the --disable-flat-float-array option with
> `opam switch create 5.1.0+flambda+nffa ocaml-variants.5.1.0+options ocaml-option-flambda ocaml-option-no-flat-float-array`

# TODO

- [ ] We should probably just rewrite the stack to have an interface. Now that we need to support upvalue objects, the base value type needs to become a reference.
- [ ] There are a couple places in the compiler we we make use of a "sliding stack" data structure that's shared among different structs. We _could_ make something like this fairly easily building off of `Vector`