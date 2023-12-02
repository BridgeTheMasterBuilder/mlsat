(lang dune 3.11)

(using menhir 2.1)

(name mlsat)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name mlsat)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune
  (containers (>= 3.12))
  (containers-data (>= 3.12))
  (iter (>= 1.7))
  (cmdliner (>= 1.2.0))
  (psq (>= 0.2.1))
  (zarith (>= 1.13)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
