;;;; advent-of-code-2024.asd

(asdf:defsystem #:advent-of-code-2024
  :description "Describe advent-of-code-2024 here"
  :author "Wojciech S. Gac <wojciech.s.gac@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :anaphora
               :array-operations
               :cl-ppcre
               :lisp-stat
               :local-time
               :trivia
               :str
               ;; :fset
	       )
  :components ((:file "package")
	       (:file "util")
               (:file "misc")
	       (:file "day01")
               ;; AoC 2023 recap
               (:file "2023/day01")))
