(lang dune 2.7)
(name otfed)
(version 0.0.0)
(generate_opam_files true)

(package
 (name otfed)
 (synopsis "Short description")
 (description "Long description")
 (depends
  (dune (>= 2.0))
  (core_kernel (>= 0.13.0))
  (ppx_deriving (>= 4.4.1))))
