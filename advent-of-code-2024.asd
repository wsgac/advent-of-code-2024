;;; advent-of-code-2024.asd

(asdf:defsystem #:advent-of-code-2024
  :description "Advent of Code 2024 solutions"
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
               ;; AoC 2023 recap
               (:module "2023"
                :components
                ((:file "day01")
                 (:file "day02")
                 (:file "day03")
                 (:file "day04")
                 (:file "day05")
                 (:file "day06")
                 (:file "day07")
                 (:file "day08")))
               ;; AoC 2024
               (:module "2024"
                :components
                ((:file "day01")
                 (:file "day02")
                 (:file "day03")
                 (:file "day04")
                 (:file "day05")
                 (:file "day06")
                 (:file "day07")
                 (:file "day08")
                 (:file "day09")
                 (:file "day10")
                 (:file "day11")
                 (:file "day12")
                 (:file "day13")
                 (:file "day14")))))
