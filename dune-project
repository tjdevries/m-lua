(lang dune 3.13)

(name mlua)

(using menhir 3.0)

(generate_opam_files true)

(cram enable)

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
