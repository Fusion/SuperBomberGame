(lang dune 2.1)
(name reasonable-fidelity)

(generate_opam_files true)

(license Apache)
(maintainers "Chris F Ravenscroft <chris@voilaweb.com>")
(authors "Chris F Ravenscroft <chris@voilaweb.com>")

(package
  (name sbg)
  (depends
    (ocaml (= 4.09))
    (sdl2 (= 0.03))
    (toml (= 5.0.0))
  )
  (synopsis "its-a-game")
)

(package
  (name sbg-test)
  (depends
    rely.lib
    sbg.lib
  )
  (synopsis "its-a-game")
)

