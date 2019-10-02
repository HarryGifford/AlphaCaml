(lang dune 1.10)

(name alphaCaml-main)

(using menhir 2.0)

(license MIT)

(package
 (name alphaCaml)
 (depends
  (ocaml
   (>= 4.07.1)))
 (synopsis "AlphaCaml")
 (description "AlphaCaml"))

(package
 (name alphaLib)
 (depends
  (ocaml
   (>= 4.07.1)))
 (synopsis "Library for usage with AlphaCaml")
 (description "Library for usage with AlphaCaml"))
