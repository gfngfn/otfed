(lang dune 2.7)
(name otfed)
(version 0.0.0)
(maintainers "Takashi Suwa")
(source (github gfngfn/otfed))
(homepage "https://github.com/gfngfn/otfed")
(generate_opam_files true)

(package
 (name otfed)
 (synopsis "Short description")
 (description "Long description")
 (depends
  (dune (>= 2.0))
  (core (>= 0.13.0))
  (ppx_deriving (>= 4.4.1))
  (uutf (>= 1.0.2))
  (alcotest (and :with-test (>= 1.4.0)))))
