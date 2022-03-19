(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/clap))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name clap)
 (synopsis "Command-Line Argument Parsing, imperative style with a consumption mechanism")
 (description "A library to describe and parse command-line arguments and generate help pages, using a simple imperative style.")
 (depends (ocaml (>= 4.07))))
