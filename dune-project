(lang dune 3.15)

(name camlox)

(generate_opam_files true)

(source
 (github hgoldstein/camlox))

(authors "Hunter Goldstein")

(maintainers "Hunter Goldstein")

(license MIT)

(package
 (name camlox)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune core core_unix ppx_deriving))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
