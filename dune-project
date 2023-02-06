(lang dune 3.6)

(name coq-of-LLBC)

(generate_opam_files true)

(source
 (github formal-land/coq-of-LLBC))

(package
 (name coq-of-LLBC)
 (synopsis "Translation of LLBC into Coq")
 (depends (ocaml (>= 4.13)) dune yojson easy_logging zarith visitors))
