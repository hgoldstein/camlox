(lang dune 3.15)

(name camlox)

(generate_opam_files true)

(source
 (github hgoldstein/camlox))

(authors "Hunter Goldstein")

(maintainers "Hunter Goldstein")

(license LICENSE)

(package
 (name camlox)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
