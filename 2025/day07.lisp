(in-package #:advent-of-code-2025.day07)

(defun parse-input (input)
  (u:parse-string-into-array input))

(defun find-start (arr)
  (loop
    :for col :below (array-dimension arr 1)
    :when (eq #\S (aref arr 0 col))
      :do (return (cons 0 col))))

(defun problem1 (&key (input *input-part-1-test*))
  (let* ((arr (parse-input input))
         (start (find-start arr)))
    (loop
      :with splits := 0
      :for row :from 1 :below (array-dimension arr 0)
      :for prev := (list start) :then curr
      :for curr := (delete-duplicates
                    (loop
                      :for (r . c) :in prev
                      :if (eq #\^ (aref arr (1+ r) c))
                        :do (incf splits)
                        :and :collect (cons (1+ r) (1- c))
                        :and :collect (cons (1+ r) (1+ c))
                      :else
                        :collect (cons (1+ r) c))
                    :test #'equal)
      :finally (return splits))))

(defun problem2 (&key (input *input-part-1-test*))
  (let* ((arr (parse-input input))
         (start (find-start arr))
         (prev (make-hash-table :test #'equal)))
    (setf (gethash (cdr start) prev) 1)
    (loop
      :for row :from 1 :below (array-dimension arr 0)
      :do (loop
            :with next := (make-hash-table :test #'equal)
            :for col :being :the hash-keys :in prev
              :using (hash-value timelines)
            :if (eq #\^ (aref arr row col))
              :do (incf (gethash (1- col) next 0) timelines)
              :and :do (incf (gethash (1+ col) next 0) timelines)
            :else
              :do (incf (gethash col next 0) timelines)
            :finally (setf prev next))
      :finally (return (reduce #'+ (a:hash-table-values prev))))))

(defparameter *input-part-1-test*
  ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 7))
