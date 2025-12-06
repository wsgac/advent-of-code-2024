(in-package #:advent-of-code-2022.day03)

(defun parse-input (input)
  (loop
    :for line :in (util:split-lines input)
    :collect (util:split-string-on-indices line (list (/ (length line) 2)))))

(defun find-common-chars (&rest strings)
  (reduce #'intersection (mapcar (a:rcurry #'coerce 'list) strings)))

(defun priority (letter)
  (cond
    ((char<= #\a letter #\z)
     (1+ (- (char-code letter) (char-code #\a))))
    ((char<= #\A letter #\Z)
     (+ 27 (- (char-code letter) (char-code #\A))))))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    :for (s1 s2) :in (parse-input input)
    :for common := (find-common-chars s1 s2)
    :when common
      :sum (priority (first common))))

(defun problem-1-parallel (&key (input *input-part-1-test*))
  (flet ((pair-to-priority (pair)
           (let ((common (apply #'find-common-chars pair)))
             (if common (priority (first common)) 0))))
    (let ((lparallel:*kernel* (lparallel:make-kernel 16)))
     (lparallel:pmap-reduce #'pair-to-priority #'+ (parse-input input)))))

(defun parse-input-2 (input)
  (util:split-lines input))

(defun problem-2 (&key (input *input-part-2-test*))
  (loop
    :for (r1 r2 r3) :on (parse-input-2 input) :by #'cdddr
    :for common := (find-common-chars r1 r2 r3)
    :when common
      :sum (priority (first common))))

(defun problem-2-parallel (&key (input *input-part-2-test*))
  (flet ((group-to-priority (group)
           (let ((common (apply #'find-common-chars group)))
             (if common (priority (first common)) 0))))
    (let ((lparallel:*kernel* (lparallel:make-kernel 16)))
     (lparallel:pmap-reduce #'group-to-priority #'+ (util:list->groups (parse-input-2 input) 3)))))

(defparameter *input-part-1-test*
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2022 3))
