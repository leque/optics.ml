(lang dune 3.4)

(name optics)
(version 0.1.0)

(generate_opam_files true)
(source (github leque/optics.ml))
(license mit)
(authors "OOHASHI, Daichi <dico.leque.comicron@gmail.com>")

(package
 (name optics)
 (synopsis "an experimental optic library for OCaml")
 (depends
   ;; for Extended indexing operators
   (ocaml (>= "4.06"))
  dune
  result))
