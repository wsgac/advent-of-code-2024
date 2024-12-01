;;;; package.lisp

(defparameter *package-local-nicknames*
  '(("a" . "alexandria")
    ("lt" . "local-time")
    ("tr" . "trivia")
    ("ana" . "anaphora")
    ("re" . "cl-ppcre")))

(defun apply-local-nicknames (package)
  (let ((pkg (find-package (symbol-name package))))
    (dolist (alias *package-local-nicknames*)
      (sb-unix::add-package-local-nickname (string-upcase (car alias))
                                           (find-package (string-upcase (cdr alias)))
                                           pkg))))

(defpackage #:advent-of-code-2024.util
  (:nicknames #:aoc2024.util #:util)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:lt #:local-time))
  (:export
   :transpose
   :histogram
   :split-lines
   :square
   :^2
   :cube
   :^3
   :join-numbers
   :position-in-2d-array
   :parse-string-into-array
   :parse-string-into-list
   :merge-plists
   :parse-integers))

(defpackage #:advent-of-code-2024.misc
  (:use #:cl))

(apply-local-nicknames '#:advent-of-code-2024.misc)

(defpackage #:advent-of-code-2024.day01
  (:nicknames #:aoc2024.day01)
  (:use #:cl #:util))

(apply-local-nicknames '#:advent-of-code-2024.day01)

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
