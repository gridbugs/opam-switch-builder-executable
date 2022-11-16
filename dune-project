(lang dune 3.5)
(name switch-builder-executable)

(generate_opam_files true)

(package
  (name switch-builder-executable)
  (allow_empty true)
  (depends
    (fmt (>= 0.9.0))
    switch-builder))
