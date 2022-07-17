(lang dune 2.9)

(version 0.1.0)

(name indigobi)

(license "GPL-3.0")

(authors "The indigobi programmers")

(maintainers "tim.arnouts@protonmail.com" "lelolartichaut@laposte.net")

(source
 (github Tim-ats-d/Indigobi))

(homepage "https://github.com/Tim-ats-d/Indigobi")

(generate_opam_files true)

(package
 (name indigobi)
 (synopsis "A cute Gemini client written in OCaml")
 (depends
  (dune
   (>= 3.0))
  (lambda-term
   (>= 3.3.0))
  (lwt
   (>= 5.6.0))
  (lwt_ssl
   (>= 1.1))
  (ppx_sexp_conv
   (>= "v0.15.1"))
  (ppx_system
   (>= "1.0"))
  (ssl
   (>= "0.5.10"))
  (ocaml
   (>= 4.13))))
