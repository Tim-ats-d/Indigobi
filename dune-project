(lang dune 2.9)

(version 0.0.1)

(name indigobi)

(license "GPL-3.0")

(authors "The indigobi programmers")

(maintainers "tim.arnouts@protonmail.com")

(source
 (github Tim-ats-d/Indigobi))

(homepage "https://github.com/Tim-ats-d/Indigobi")

(generate_opam_files true)

(package
 (name indigobi)
 (synopsis "A cute Gemini client written in OCaml")
 (depends
  dune
  lambda-term
  (ocaml
   (>= 4.11))))
