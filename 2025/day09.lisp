(in-package #:advent-of-code-2025.day09)

(defun parse-input (input)
  (loop
    :for row :in (u:split-lines input)
    :collect (u:parse-integers row :sep '(#\,))))

(trivia:defun-match* rectangle-area (p1 p2)
    (((list p1r p1c) (list p2r p2c))
     (* (1+ (abs (- p1r p2r)))
        (1+ (abs (- p1c p2c))))))

(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :for (p1 . p1t) :on (parse-input input)
    :maximize (loop
               :for p2 :in p1t
               :maximize (rectangle-area p1 p2))))

(defun rectangle-in-grid? (p1 p2 grid)
  nil)

(defun problem2 (&key (input *input-part-1-test*))
  (let ((grid (parse-input input)))
   (loop
     :for (p1 . p1t) :on grid
     :maximize (loop
                 :for p2 :in p1t
                 :when (rectangle-in-grid? p1 p2 grid)
                   :maximize (rectangle-area p1 p2)))))

(defparameter *input-part-1-test*
  "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 9))
