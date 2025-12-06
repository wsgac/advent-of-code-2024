(in-package #:advent-of-code-2022.day04)

(defun parse-input (input)
  (mapcar #'util:parse-ranges (util:split-lines input)))

(trivia:defun-match fully-contains? (pair)
  ((list (cons from1 to1) (cons from2 to2))
   (or (<= from1 from2 to2 to1)
       (<= from2 from1 to1 to2))))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    :for pair :in (parse-input input)
    :count (fully-contains? pair)))

(defun problem-1-parallel (&key (input *input-part-1-test*))
  (let ((pairs (parse-input input))
        (lparallel:*kernel* (lparallel:make-kernel 16)))
   (lparallel:pcount-if #'fully-contains? pairs)))

(trivia:defun-match overlap? (pair)
  ((list (cons from1 to1) (cons from2 to2))
   (or (and (<= from1 to2) (>= to1 from2))
       (and (<= from2 to1) (>= to2 from1)))))

(defun problem-2 (&key (input *input-part-2-test*))
  (loop
    :for pair :in (parse-input input)
    :count (overlap? pair)))

(defun problem-2-parallel (&key (input *input-part-1-test*))
  (let ((pairs (parse-input input))
        (lparallel:*kernel* (lparallel:make-kernel 16)))
   (lparallel:pcount-if #'overlap? pairs)))

(defparameter *input-part-1-test*
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2022 4))
