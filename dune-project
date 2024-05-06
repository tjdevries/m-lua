(lang dune 3.8)

(name mlua)

(using menhir 2.1)
(generate_opam_files true)

(source
 (github tjdevries/mlua))

(authors "TJ DeVries")
(maintainers "TJ DeVries")

(license MIT)

(package
 (name mlua)
 (synopsis "Lua Interpreter")
 (description "For Fun")
 (depends ocaml dune)
 (tags
  (topics lua fun mlua)))
