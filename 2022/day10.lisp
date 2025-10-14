(in-package #:advent-of-code-2022.day10)

;; Solution

(defun parse-input (input)
  (loop
    :for line :in (util:split-lines input)
    :for (op arg) := (uiop:split-string line)
    :collect `(,(a:make-keyword (string-upcase op))
               ,@(when arg
                   (list (parse-integer arg))))))

(defun run-ops (ops)
  (loop
    :with x := 1
    :with trace := (list 1)
    :for (op arg) :in ops
    :do (case op
          (:noop
           (push x trace))
          (:addx
           (push x trace)
           (push (incf x arg) trace)))
    :finally (return (nreverse trace))))

(defun problem-1 (&key (input *input-part-1-test*))
  (let* ((trace-list (run-ops (parse-input input)))
        (n (length trace-list))
        (trace (make-array `(,n) :initial-contents trace-list)))
    (loop
      :for i :from 19 :by 40 :below n
      :sum (* (1+ i) (aref trace i))
      ;; :collect (list (1+ i) (aref trace i))
      )))

(defun problem-2 (&key (input *input-part-2-test*))
  (with-output-to-string (s)
    (loop
      :for i :from 0 :below 240
      :for x :in (run-ops (parse-input input))
      :when (and  (zerop (mod i 40)))
        :do (terpri s)
      :if (<= (1- x) (mod i 40) (1+ x))
        :do (write-char #\# s)
      :else
        :do (write-char #\. s))))

;; Data

(defparameter *input-part-1-test-1*
  "noop
addx 3
addx -5")

(defparameter *input-part-1-test-2*
  "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  "noop
noop
noop
addx 4
addx 1
addx 5
addx 1
addx 5
noop
addx -1
addx -6
addx 11
noop
noop
noop
noop
addx 6
addx 5
noop
noop
noop
addx -30
addx 34
addx 2
addx -39
noop
addx 5
addx 2
addx 19
addx -18
addx 2
addx 5
addx 2
addx 3
noop
addx 2
addx 3
noop
addx 2
addx 3
noop
addx 2
addx 3
noop
addx 2
addx -15
addx -22
noop
noop
addx 5
addx 2
noop
noop
addx 14
addx -11
addx 5
addx 2
addx 3
noop
addx 2
addx -16
addx 17
addx 2
addx 5
addx 2
addx -6
addx -25
addx 35
addx 1
addx -36
addx 1
addx 22
addx -19
addx 5
addx 2
noop
noop
addx 5
noop
noop
noop
addx 1
addx 4
noop
noop
noop
addx 5
noop
addx 1
addx 2
addx 3
addx 4
addx -34
addx 21
addx -24
addx 2
addx 5
addx 7
addx -6
addx 2
addx 30
addx -23
addx 10
addx -9
addx 2
addx 2
addx 5
addx -12
addx 13
addx 2
addx 5
addx 2
addx -12
addx -24
addx -1
noop
addx 3
addx 3
addx 1
addx 5
addx 21
addx -16
noop
addx 19
addx -18
addx 2
addx 5
addx 2
addx 3
noop
addx 3
addx -1
addx 1
addx 2
addx -18
addx 1
noop")
