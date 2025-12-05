(in-package #:advent-of-code-2025.day05)

(defun parse-input (input)
  (destructuring-bind (ranges-raw ingredients-raw) (str:split "

" input)
    (let ((ranges (loop
                    :for row :in (util:split-lines ranges-raw)
                    :collect (util:parse-integers row :sep '(#\-))))
          (ingredients (mapcar #'parse-integer (util:split-lines ingredients-raw))))
      (list ranges ingredients))))

(defun fresh? (ranges ingredient)
  (loop
    :for (l r) :in ranges
    :when (<= l ingredient r)
      :do (return t)))

(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :with (ranges ingredients) := (parse-input input)
    :for ingredient :in ingredients
    :count (fresh? ranges ingredient)))

(defun merge-ranges (ranges)
  (labels ((%merge (ranges)
             (if (or (null ranges)
                     (= 1 (length ranges)))
                 ranges
                 (destructuring-bind ((e1l e1h) (e2l e2h) &rest tail)
                     ranges
                   (if (> e2l e1h)
                       (cons (first ranges)
                             (%merge (rest ranges)))
                       (%merge (cons (list e1l (max e1h e2h)) tail)))))))
    (let* ((sorted (sort ranges #'< :key #'first))
           (merged (%merge sorted)))
      merged)))

(defun problem2 (&key (input *input-part-1-test*))
  (let ((merged-ranges (merge-ranges (first (parse-input input)))))
    (loop
      :for (l h) :in merged-ranges
      :sum (- h l -1))))

(defparameter *input-part-1-test*
  "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 5))
