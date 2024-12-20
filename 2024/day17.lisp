(in-package #:advent-of-code-2024.day17)

(defun parse-input (input)
  (let* ((parts (str:split "

" input))
         (registers (mapcar (lambda (l) (parse-integer
                                         (third (uiop:split-string l))))
                            (util:split-lines (first parts))))
         (program (util:extract-integers (second parts))))
    `(,@registers ,program)))

(defclass cpu ()
  ((a :initarg :a
      :accessor a)
   (b :initarg :b
      :accessor b)
   (c :initarg :c
      :accessor c)
   (pc :initarg :pc
       :initform 0
       :accessor pc)
   (program :initarg :program
            :accessor program)
   (output :initarg :output
           :initform nil
           :accessor output)))

(define-condition end-of-program () ())

(defmethod single-step ((cpu cpu))
  (with-slots (pc program) cpu
    (if (>= pc (length program))
        (error 'end-of-program)
        (let ((op (aref program pc))
              (operand (aref program (1+ pc))))
          (ecase op
            (0 (adv cpu operand))
            (1 (bxl cpu operand))
            (2 (bst cpu operand))
            (3 (jnz cpu operand))
            (4 (bxc cpu operand))
            (5 (out cpu operand))
            (6 (bdv cpu operand))
            (7 (cdv cpu operand)))))))

(defmethod adv ((cpu cpu) operand)
  (with-slots (a pc) cpu
    (setf a (truncate a (expt 2 (combo cpu operand))))
    (incf pc 2)))

(defmethod bxl ((cpu cpu) operand)
  (with-slots (b pc) cpu
    (setf b (logxor b operand))
    (incf pc 2)))

(defmethod bst ((cpu cpu) operand)
  (with-slots (b pc) cpu
    (setf b (mod (combo cpu operand) 8))
    (incf pc 2)))

(defmethod jnz ((cpu cpu) operand)
  (with-slots (a pc) cpu
    (if (zerop a)
        (incf pc 2)
        (setf pc operand))))

(defmethod bxc ((cpu cpu) operand)
  (with-slots (b c pc) cpu
    (setf b (logxor b c))
    (incf pc 2)))

(defmethod out ((cpu cpu) operand)
  (with-slots (output pc) cpu
   (push (mod (combo cpu operand) 8) output)
   (incf pc 2)))

(defmethod bdv ((cpu cpu) operand)
  (with-slots (a b pc) cpu
    (setf b (truncate a (expt 2 (combo cpu operand))))
    (incf pc 2)))

(defmethod cdv ((cpu cpu) operand)
  (with-slots (a c pc) cpu
    (setf c (truncate a (expt 2 (combo cpu operand))))
    (incf pc 2)))

(defmethod combo ((cpu cpu) operand)
  (ecase operand
    ((0 1 2 3) operand)
    (4 (a cpu))
    (5 (b cpu))
    (6 (c cpu))
    (7 nil)))

#+(or)
(mapcar #'combo (a:iota 8))
#+(or)
(combo 8)

(defun problem-1 (&key (input *input-part-1-test*))
  (destructuring-bind (a b c program)
      (parse-input input)
    (let* ((cpu (make-instance 'cpu :a a :b b :c c :program (coerce program 'vector))))
      (loop
        do (handler-case (single-step cpu)
             (end-of-program ()
               (return))))
      (format nil "~{~d~^,~}" (reverse (output cpu))))))

(defun run-program (a program)
  (let* ((cpu (make-instance 'cpu :a a :b 0 :c 0 :program (coerce program 'vector))))
    (loop
      do (handler-case (single-step cpu)
           (end-of-program ()
             (return))))
    (reverse (output cpu))))

(defun find-a-ending-with (a-start digits program)
  (loop
    for a from a-start
    for output = (run-program a program)
    until (equal digits output)
    finally (return a)))

(defun problem-2 (&key (input *input-part-2-test*))
  (let ((program (fourth (parse-input input))))
    (loop
      with digits = nil
      with a = 0
      for digit in (reverse program)
      do (push digit digits)
      do (setf a (* 8 (find-a-ending-with a digits program)))
      finally (return (/ a 8)))))



(defparameter *input-part-1-test*
  "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(defparameter *input-part-2-test*
  "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0")

(defparameter *input*
  "Register A: 61657405
Register B: 0
Register C: 0

Program: 2,4,1,2,7,5,4,3,0,3,1,7,5,5,3,0")
