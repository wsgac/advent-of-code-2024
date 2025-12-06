(in-package #:advent-of-code-2025.day06)

(defun parse-input (input)
  (loop
    :for i :from 0
    :for row :in (reverse (u:split-lines input))
    :if (zerop i)
      :collect (mapcar #'symbol-function (u:parse-symbols row)) :into args
    :else
      :collect (u:parse-integers row) :into args
    :finally (return (u:transpose args))))

(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :for (f . args) :in (parse-input input)
    :sum (apply f args)))

(defun parse-input2 (input)
  (let ((transposed (u:2d-array->string
                     (u:array-transpose (u:parse-string-into-array input)))))
    (loop
      :for group :in (re:split "\\n\\s*\\n" transposed)
      :collect (loop
                 :with rows := (u:split-lines group)
                 :with op := (symbol-function (find-symbol (str:s-last (first rows))))
                 :for row :in rows
                 :collect (parse-integer row :junk-allowed t) :into args
                 :finally (return (cons op args))))))

(defun problem2 (&key (input *input-part-1-test*))
  (loop
    :for (f . args) :in (parse-input2 input)
    :sum (apply f args)))

(defparameter *input-part-1-test*
  "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 6))
