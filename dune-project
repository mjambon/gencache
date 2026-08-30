(lang dune 3.18)

(name gencache)

(generate_opam_files true)

(source
 (github mjambon/gencache))

(authors "Martin Jambon <martin@mjambon.com>")

(maintainers "Martin Jambon <martin@mjambon.com>")

(license AGPL-3.0-only)

(documentation https://mjambon.github.io/gencache)

(package
 (name gencache)
 (synopsis "A general-purpose generational cache")
 (description "A general-purpose generational cache")
 (depends
  (ocaml (>= 4.14.0))
  (testo :with-test)
  (odoc (and (>= 3.2.1) :with-doc))
 )
 (tags
  ("cache" "data structures")
 )
)
