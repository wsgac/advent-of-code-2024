(in-package #:advent-of-code-2025.day02)

(defun parse-input (input)
  (loop
    :for range :in (uiop:split-string input :separator '(#\,))
    :collect (util:parse-integers range :sep '(#\-))))

(defun invalid? (num)
  "Checks whether NUM is composed of the same sequence of digits repeated
twice. This only has a chance of being true for numbers with an even
number of digits."
  (let ((ndigits (util:digit-count num)))
    (and (evenp ndigits)
         (multiple-value-bind (l r)
             (truncate num (expt 10 (/ ndigits 2)))
           (= l r)))))

(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :for (low high) :in (parse-input input)
    :sum (loop
           :for num :from low :to high
           :when (invalid? num)
             :sum num)))

(defun generated-by-sub? (num factor)
  "Check whether NUM can be expressed as 2 or more sequences of digits of
length FACTOR."
  (loop
    :with d := (expt 10 factor)
    :for n := (truncate num d) :then (truncate n d)
    :while (plusp n)
    :for prev := (rem num d) :then curr
    :for curr := (rem n d)
    :unless (= curr prev)
      :do (return nil)
    :finally (return t)
    ))

(defun invalid>=2? (num)
  "Checks whether NUM can be composed of a sequence of digits repeated
twice or more. It follows that the only viable sequence lengths are
factors of the number of digits of NUM, except that number itself."
  (loop
    :for factor :in (cons 1 (util:factors (util:digit-count num)))
    :when (generated-by-sub? num factor)
      :do (return t)))

(defun problem2 (&key (input *input-part-1-test*))
  (loop
    :for (low high) :in (parse-input input)
    :sum (loop
           :for num :from low :to high
           :when (and (>= num 10) (invalid>=2? num))
             :sum num)))

(defparameter *input-part-1-test*
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (util:get-problem-data 2025 2))
