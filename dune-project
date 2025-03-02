(lang dune 3.17)
(using menhir 3.0)

(name diablo)

(generate_opam_files true)

(source
 (github diablo-lang/diablo_llvm))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://diablo-lang.org)

(package
 (name diablo)
 (synopsis "The Diablo Programming Language")
 (description "Diablo is a statically typed, functional language designed to augment research in information security and the natural sciences.")
 (depends ocaml dune alcotest llvm menhir)
 (tags
  (functional "programming language" compiler interpreter)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
