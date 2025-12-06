(in-package #:advent-of-code-2022.day01)

(defun parse-input (input)
  (mapcar (alexandria:rcurry #'util:parse-integers :sep '(#\newline)) (str:split "

" input)))

(defun problem-1 (&key (input *input-part-1-test*))
  "Treat `input` as blank-line-delimited list of items, each of which is
a list of integer calorie values. Return the highest total caloric
value of any such list."
  (let ((parsed (parse-input input)))
    (loop
      :for calorie-list :in parsed
      :maximize (apply #'+ calorie-list))))

(defun problem-2 (&key (input *input-part-2-test*))
  "Treat `input` as blank-line-delimited list of items, each of which is
a list of integer calorie values. Return the sum of the three highest
total caloric values from among them."
  (let ((parsed (parse-input input)))
    (loop
      :for calorie-list :in parsed
      :collect (apply #'+ calorie-list) :into sums
      :finally (return (apply #'+ (subseq (sort sums #'>) 0 3))))))

(defparameter *input-part-1-test*
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2022 1))
