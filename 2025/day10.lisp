(in-package #:advent-of-code-2025.day10)

(defun parse-diagram (diagram-str)
  (map 'vector (a:curry #'char= #\#) (subseq diagram-str 1 (1- (length diagram-str)))))

#+(or)
(parse-diagram "[.##.]")

(defun parse-buttons (buttons)
  (loop
    :for buttons-str :in buttons
    :collect (u:parse-integers (subseq buttons-str 1 (1- (length buttons-str)))
                               :sep '(#\,))))

#+(or)
(parse-buttons '("(3)" "(1,3)" "(2)" "(2,3)" "(0,2)" "(0,1)"))

(defun parse-joltage (joltage-str)
  (coerce (u:parse-integers (subseq joltage-str 1 (1- (length joltage-str)))
                            :sep '(#\,))
          'vector))

(defun parse-input (input)
  (loop
    :for row-str :in (u:split-lines input)
    :for row := (uiop:split-string row-str)
    :collect (let ((diagram-str (first row))
                   (buttons (butlast (rest row)))
                   (joltage-str (a:lastcar row)))
               (list :diagram (parse-diagram diagram-str)
                     :buttons (parse-buttons buttons)
                     :joltage (parse-joltage joltage-str)))))

(defun apply-button (diagram button)
  (let ((copy (copy-seq diagram)))
    (loop
      :for toggle :in button
      :do (notf (aref copy toggle))
      :finally (return copy))))

(defun bfs-1 (goal buttons)
  (flet ((done? (diag)
           (equalp diag goal)))
    (let ((start (make-array (length goal) :initial-element nil))
          (frontier (sb-concurrency:make-queue)))
      (loop
        :initially (sb-concurrency:enqueue (cons start 0) frontier)
        :while (not (sb-concurrency:queue-empty-p frontier))
        :for (curr . count) := (sb-concurrency:dequeue frontier)
        :do (loop
              :for button :in buttons
              :for next := (apply-button curr button)
              :when (done? next)
                :do (return-from bfs-1 (1+ count))
              :do (sb-concurrency:enqueue (cons next (1+ count)) frontier))))))

(defun problem1 (&key (input *input-part-1-test*))
  (let ((machines (parse-input input)))
    (loop
      :for machine :in machines
      :sum (bfs-1 (getf machine :diagram) (getf machine :buttons)))))

;; Scratch this. This is silly!
;; (defun bfs-2 (goal buttons)
;;   (flet ((done? (jolt)
;;            (equalp jolt goal))
;;          (count-button (jolt button)
;;            (let ((copy (copy-seq jolt)))
;;              (loop
;;                :for press :in button
;;                :do (incf (aref copy press))
;;                :finally (return copy)))))
;;     (let ((start (make-array (length goal) :initial-element 0))
;;           (frontier (sb-concurrency:make-queue)))
;;       (loop
;;         :initially (sb-concurrency:enqueue (cons start 0) frontier)
;;         :while (not (sb-concurrency:queue-empty-p frontier))
;;         :for (curr . count) := (sb-concurrency:dequeue frontier)
;;         :do (loop
;;               :for button :in buttons
;;               :for next := (count-button curr button)
;;               :when (done? next)
;;                 :do (return-from bfs-2 (1+ count))
;;               :do (sb-concurrency:enqueue (cons next (1+ count)) frontier))))))

(defun machine->problem (machine)
  (let* ((buttons (getf machine :buttons))
         (joltage (getf machine :joltage))
         (vars (loop
                 :for i :from 1 :to (length buttons)
                 :collect (intern (format nil "X~d" i))))
         (constraints (loop
                        :for i :below (length joltage)
                        :collect (loop
                                   :for b :in buttons
                                   :for var :in vars
                                   :when (member i b)
                                     :collect var :into constraint
                                   :finally (return `(= (+ ,@constraint) ,(nth i joltage)))))))
    `(linear-programming:make-linear-problem (min (+ ,@vars)) ,@constraints)
    ))

(defun problem2 (&key (input *input-part-1-test*))
  (let ((machines (parse-input input)))
    (loop
      :for machine :in machines
      :for problem := (eval (machine->problem machine))
      :for solution := (if (linear-programming:problem-constraints problem)
                           (truncate (linear-programming:solution-objective-value
                                      (linear-programming-glpk:glpk-solver problem)))
                           (progn (print machine)
                                  (reduce #'+ (mapcar #'second
                                                      (remove-duplicates
                                                       (linear-programming:problem-var-bounds problem)
                                                       :test #'equalp)))))
      :sum solution)))

(defparameter *input-part-1-test*
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 10))
