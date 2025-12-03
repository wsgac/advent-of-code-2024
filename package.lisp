;;;; package.lisp

(defpackage #:advent-of-code.util
  (:nicknames #:aoc.util #:util)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:lt #:local-time)
                    (#:re #:cl-ppcre))
  (:export
   :transpose
   :histogram
   :split-lines
   :split-string-on-indices
   :list->groups
   :parse-ranges
   :square
   :^2
   :cube
   :^3
   :join-numbers
   :position-in-2d-array
   :parse-string-into-array
   :parse-string-into-list
   :merge-plists
   :parse-integers
   :choose-combinations-replacing
   :choose-combinations-not-replacing
   :concat-numbers
   :number->digits
   :digits->number
   :char-arr->num-arr
   :extract-integers
   :array-loop
   :2d-array->string
   :2d-array->png
   :within-array
   :vec+
   :aref*
   :manhattan
   :bron-kerbosch
   :compose-n
   :vector-push-n
   ;; Priority Queue
   :priority-queue
   :priority-queue-heap
   :priority-queue-list
   :pq-push
   :pq-pop
   :pq-empty?
   :character-list
   :file-at-once
   :with-output-to-multiple
   ;; fmap-queue
   :fset-queue
   :enqueue
   :dequeue
   :size
   :empty?
   :digit-count
   :factors))

(defparameter *package-local-nicknames*
  '(("a" . "alexandria")
    ("lt" . "local-time")
    ("tr" . "trivia")
    ("ana" . "anaphora")
    ("re" . "cl-ppcre")
    ("s" . "serapeum")
    ("u" . "util")
    ("o" . "onlisp")))

(defun apply-local-nicknames (package)
  (let ((pkg (find-package (symbol-name package))))
    (dolist (alias *package-local-nicknames*)
      (sb-unix::add-package-local-nickname (string-upcase (car alias))
                                           (find-package (string-upcase (cdr alias)))
                                           pkg))))

(defpackage #:advent-of-code-2024.onlisp
  (:nicknames #:aoc2024.onlisp #:onlisp)
  (:use #:cl)
  (:export
   ))

(apply-local-nicknames '#:advent-of-code-2024.onlisp)

(defpackage #:advent-of-code-2024.misc
  (:use #:cl))

(apply-local-nicknames '#:advent-of-code-2024.misc)

(defpackage #:advent-of-code-2024.sicp
  (:use #:cl))

(apply-local-nicknames '#:advent-of-code-2024.sicp)

;; 2024

(defpackage #:advent-of-code-2024.day01
  (:nicknames #:aoc2024.day01)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day01)

(defpackage #:advent-of-code-2024.day02
  (:nicknames #:aoc2024.day02)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day02)

(defpackage #:advent-of-code-2024.day03
  (:nicknames #:aoc2024.day03)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day03)

(defpackage #:advent-of-code-2024.day04
  (:nicknames #:aoc2024.day04)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day04)

(defpackage #:advent-of-code-2024.day05
  (:nicknames #:aoc2024.day05)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day05)

(defpackage #:advent-of-code-2024.day06
  (:nicknames #:aoc2024.day06)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day06)

(defpackage #:advent-of-code-2024.day07
  (:nicknames #:aoc2024.day07)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day07)

(defpackage #:advent-of-code-2024.day08
  (:nicknames #:aoc2024.day08)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day08)

(defpackage #:advent-of-code-2024.day09
  (:nicknames #:aoc2024.day09)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day09)

(defpackage #:advent-of-code-2024.day10
  (:nicknames #:aoc2024.day10)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day10)

(defpackage #:advent-of-code-2024.day11
  (:nicknames #:aoc2024.day11)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day11)

(defpackage #:advent-of-code-2024.day12
  (:nicknames #:aoc2024.day12)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day12)

(defpackage #:advent-of-code-2024.day13
  (:nicknames #:aoc2024.day13)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day13)

(defpackage #:advent-of-code-2024.day14
  (:nicknames #:aoc2024.day14)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day14)

(defpackage #:advent-of-code-2024.day15
  (:nicknames #:aoc2024.day15)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day15)

(defpackage #:advent-of-code-2024.day16
  (:nicknames #:aoc2024.day16)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day16)

(defpackage #:advent-of-code-2024.day17
  (:nicknames #:aoc2024.day17)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day17)

(defpackage #:advent-of-code-2024.day18
  (:nicknames #:aoc2024.day18)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day18)

(defpackage #:advent-of-code-2024.day19
  (:nicknames #:aoc2024.day19)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day19)

(defpackage #:advent-of-code-2024.day20
  (:nicknames #:aoc2024.day20)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day20)

(defpackage #:advent-of-code-2024.day21
  (:nicknames #:aoc2024.day21)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day21)

(defpackage #:advent-of-code-2024.day22
  (:nicknames #:aoc2024.day22)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day22)

(defpackage #:advent-of-code-2024.day23
  (:nicknames #:aoc2024.day23)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day23)

(defpackage #:advent-of-code-2024.day24
  (:nicknames #:aoc2024.day24)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day24)

(defpackage #:advent-of-code-2024.day25
  (:nicknames #:aoc2024.day25)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day25)

;; AoC 2025

(defpackage #:advent-of-code-2025.day01
  (:nicknames #:aoc2025.day01)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2025.day01)

;; AoC 2023 recap - renewed attempts at solving some problems from 2023

(defpackage #:advent-of-code-2023.day01
  (:nicknames #:aoc2023.day01)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day01)

(defpackage #:advent-of-code-2023.day02
  (:nicknames #:aoc2023.day02)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day02)

(defpackage #:advent-of-code-2023.day03
  (:nicknames #:aoc2023.day03)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day03)

(defpackage #:advent-of-code-2023.day04
  (:nicknames #:aoc2023.day04)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day04)

(defpackage #:advent-of-code-2023.day05
  (:nicknames #:aoc2023.day05)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day05)

(defpackage #:advent-of-code-2023.day06
  (:nicknames #:aoc2023.day06)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day06)

(defpackage #:advent-of-code-2023.day07
  (:nicknames #:aoc2023.day07)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day07)

(defpackage #:advent-of-code-2023.day08
  (:nicknames #:aoc2023.day08)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2023.day08)

;; AoC 2022 recap

(defpackage #:advent-of-code-2022.day01
  (:nicknames #:aoc2022.day01)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day01)

(defpackage #:advent-of-code-2022.day02
  (:nicknames #:aoc2022.day02)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day02)

(defpackage #:advent-of-code-2022.day03
  (:nicknames #:aoc2022.day03)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day03)

(defpackage #:advent-of-code-2022.day04
  (:nicknames #:aoc2022.day04)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day04)

(defpackage #:advent-of-code-2022.day05
  (:nicknames #:aoc2022.day05)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day05)

(defpackage #:advent-of-code-2022.day06
  (:nicknames #:aoc2022.day06)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day06)

(defpackage #:advent-of-code-2022.day07
  (:nicknames #:aoc2022.day07)
  (:use #:cl))

(apply-local-nicknames '#:advent-of-code-2022.day07)

(defpackage #:advent-of-code-2022.day08
  (:nicknames #:aoc2022.day08)
  (:use #:cl))

(apply-local-nicknames '#:advent-of-code-2022.day08)

(defpackage #:advent-of-code-2022.day09
  (:nicknames #:aoc2022.day09)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day09)

(defpackage #:advent-of-code-2022.day10
  (:nicknames #:aoc2022.day10)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day10)

(defpackage #:advent-of-code-2022.day11
  (:nicknames #:aoc2022.day11)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day11)

(defpackage #:advent-of-code-2022.day12
  (:nicknames #:aoc2022.day12)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day12)

(defpackage #:advent-of-code-2022.day13
  (:nicknames #:aoc2022.day13)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day13)

(defpackage #:advent-of-code-2022.day14
  (:nicknames #:aoc2022.day14)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day14)

(defpackage #:advent-of-code-2022.day15
  (:nicknames #:aoc2022.day15)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2022.day15)

;; 2025

(defpackage #:advent-of-code-2025.day01
  (:nicknames #:aoc2025.day01)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2025.day01)

(defpackage #:advent-of-code-2025.day02
  (:nicknames #:aoc2025.day02)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2025.day02)

(defpackage #:advent-of-code-2025.day03
  (:nicknames #:aoc2025.day03)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2025.day03)

(defpackage #:sudoku
  ;; (:nicknames #:aoc2022.day10)
  (:use #:cl #:util))

(apply-local-nicknames '#:sudoku)

(defpackage #:m3u-downloader
  (:use #:cl))

(apply-local-nicknames '#:m3u-downloader)

(defpackage #:hdl
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:ser #:serapeum)))
