(in-package #:advent-of-code-2022.day11)

;; Solution

(defstruct monkey
  (items (sb-concurrency:make-queue))
  operation
  where-to-throw
  (inspections 0))

(defun process-monkey (raw-monkey &key (relief t))
  (let* ((rows (util:split-lines raw-monkey))
         (items (util:parse-integers (second rows)))
         (op-str (uiop:split-string (second (str:split " = " (third rows)))))
         (divisor (first (util:parse-integers (fourth rows))))
         (true-target (first (util:parse-integers (fifth rows))))
         (false-target (first (util:parse-integers (sixth rows)))))
    (flet ((make-lambda (op-str)
                 (destructuring-bind (a1 op a2)
                     (mapcar #'read-from-string op-str)
                   (coerce `(lambda (,a1)
                              (if ,relief
                                  (truncate (,op ,a1 ,a2) 3)
                                  (,op ,a1 ,a2)))
                           'function))))
      (list (make-lambda op-str)
            (lambda (x)
              (if (zerop (mod x divisor))
                  true-target
                  false-target))
            items))))

(defun parse-input (input &key (relief t))
  (loop
    :for raw-monkey :in (str:split "

" input)
    :for (operation decision items) := (process-monkey raw-monkey :relief relief)
    :for monkey := (make-monkey :operation operation
                                :where-to-throw decision)
    :do (dolist (item items)
          (sb-concurrency:enqueue item (monkey-items monkey)))
    :collect monkey))

(defun play-round (monkeys)
  (dolist (monkey monkeys)
    (monkey-business monkey monkeys))
  monkeys)

(defun monkey-business (monkey monkeys)
  (with-slots (items operation where-to-throw inspections) monkey
   (loop
     :until (sb-concurrency:queue-empty-p items)
     :for item := (sb-concurrency:dequeue items)
     :for worry-adjusted := (funcall operation item)
     :for throw-to := (funcall where-to-throw worry-adjusted)
     :do (incf inspections)
     :do (sb-concurrency:enqueue worry-adjusted
                                 (monkey-items (nth throw-to monkeys))))))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    :for monkeys := (parse-input input) :then (play-round monkeys)
    :repeat 20
    :finally (return (reduce #'* (subseq (sort monkeys #'> :key #'monkey-inspections) 0 2)
                             :key #'monkey-inspections))))

(defun problem-2 (&key (input *input-part-1-test*))
  (loop
    :for monkeys := (parse-input input :relief nil) :then (play-round monkeys)
    :repeat 10000
    :finally (return (reduce #'* (subseq (sort monkeys #'> :key #'monkey-inspections) 0 2)
                             :key #'monkey-inspections))))

;; Data

(defparameter *input-part-1-test*
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")


(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  "Monkey 0:
  Starting items: 91, 66
  Operation: new = old * 13
  Test: divisible by 19
    If true: throw to monkey 6
    If false: throw to monkey 2

Monkey 1:
  Starting items: 78, 97, 59
  Operation: new = old + 7
  Test: divisible by 5
    If true: throw to monkey 0
    If false: throw to monkey 3

Monkey 2:
  Starting items: 57, 59, 97, 84, 72, 83, 56, 76
  Operation: new = old + 6
  Test: divisible by 11
    If true: throw to monkey 5
    If false: throw to monkey 7

Monkey 3:
  Starting items: 81, 78, 70, 58, 84
  Operation: new = old + 5
  Test: divisible by 17
    If true: throw to monkey 6
    If false: throw to monkey 0

Monkey 4:
  Starting items: 60
  Operation: new = old + 8
  Test: divisible by 7
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 5:
  Starting items: 57, 69, 63, 75, 62, 77, 72
  Operation: new = old * 5
  Test: divisible by 13
    If true: throw to monkey 7
    If false: throw to monkey 4

Monkey 6:
  Starting items: 73, 66, 86, 79, 98, 87
  Operation: new = old * old
  Test: divisible by 3
    If true: throw to monkey 5
    If false: throw to monkey 2

Monkey 7:
  Starting items: 95, 89, 63, 67
  Operation: new = old + 2
  Test: divisible by 2
    If true: throw to monkey 1
    If false: throw to monkey 4")
