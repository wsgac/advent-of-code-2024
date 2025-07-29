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
               :lparallel
               :local-time
               :trivia
               :str
               :serapeum
               :zpng
               ;; :fset
	       )
  :components ((:file "package")
               (:file "onlisp")
	       (:file "util")
               (:file "misc")
               (:file "sicp")
               ;; AoC 2022 recap
               (:module "2022"
                :components
                ((:file "day01")
                 (:file "day02")
                 (:file "day03")
                 (:file "day04")
                 (:file "day05")
                 (:file "day06")
                 (:file "day07")))
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
                 (:file "day14")
                 (:file "day15")
                 (:file "day16")
                 (:file "day17")
                 (:file "day18")
                 (:file "day19")
                 (:file "day20")
                 (:file "day21")
                 (:file "day22")
                 (:file "day23")
                 (:file "day24")
                 (:file "day25")))))
