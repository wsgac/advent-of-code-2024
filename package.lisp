;;;; package.lisp

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
   :join-numbers
   :position-in-2d-array
   :parse-string-into-array
   :parse-string-into-list))

(defpackage #:advent-of-code-2024.day01
  (:nicknames #:aoc2024.day01)
  (:use #:cl #:util))

