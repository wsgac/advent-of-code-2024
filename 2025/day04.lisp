(in-package #:advent-of-code-2025.day04)

(defun parse-input (input)
  (util:parse-string-into-array input))

(defun generate-deltas ()
  (loop
    :for row :from -1 :to 1
    :append (loop
              :for col :from -1 :to 1
              :unless (and (zerop row) (zerop col))
                :collect (cons row col))))

(defun roll-movable? (arr row col)
  (loop
    :for (dr . dc) :in (generate-deltas)
    :for el := (ignore-errors (aref arr (+ row dr) (+ col dc)))
    :count (and el (char= el #\@)) :into neighbors
    :finally (return (< neighbors 4))))

(defun find-removable (arr)
  (let ((movable 0)
        (movable-points nil))
   (util:array-loop (arr (row col) :item el)
     (when (and (char= el #\@)
                (roll-movable? arr row col))
       (incf movable)
       (push (cons row col) movable-points)))
    (values movable movable-points)))

(defun problem1 (&key (input *input-part-1-test*))
  (let ((arr (parse-input input)))
    (nth-value 0 (find-removable arr))))

(defun remove-movable (arr movable-points)
  (loop
    :for (row . col) :in movable-points
    :do (setf (aref arr row col) #\.)))

(defun problem2 (&key (input *input-part-1-test*))
  (let ((arr (parse-input input))
        (moved 0))
    (loop
      :for (movable movable-points) := (multiple-value-list (find-removable arr))
      :until (zerop movable)
      :do (incf moved movable)
      :do (remove-movable arr movable-points))
    moved))

(defparameter *input-part-1-test*
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 4))
