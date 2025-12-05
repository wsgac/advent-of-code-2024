(in-package #:advent-of-code-2025.day03)

(defun parse-input (input)
  (loop
    :for bank :in (util:split-lines input)
    :collect (loop
               :for bat :across bank
               :collect (digit-char-p bat))))

(defun joltage (bank)
  (loop
    :for b1 :on bank
    :maximize (loop
                :for b2 :in (rest b1)
                :when b2
                  :maximize (+ (* 10 (first b1)) b2))))

(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :for bank :in (parse-input input)
    :sum (joltage bank)))

(defun find-max (list)
  (loop
    :with ii := -2
    :with mm := -1
    :for i :from 0
    :for el :in list
    :when (> el mm)
      :do (setf ii i mm el)
    :finally (return (cons ii mm))))

(defun joltage-n (bank n)
  (labels ((joltage (bank n)
            (if (= n 1)
                (list (apply #'max bank))
                (let ((digits (length bank)))
                  (destructuring-bind (i . d)
                      (find-max (subseq bank 0 (- digits n -1)))
                    (cons d (joltage (subseq bank (1+ i)) (1- n))))))))
    (util:digits->number (joltage bank n))))

(defun problem2 (&key (input *input-part-1-test*))
  (loop
    :for bank :in (parse-input input)
    :sum (joltage-n bank 12)))

(defparameter *input-part-1-test*
  "987654321111111
811111111111119
234234234234278
818181911112111")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (util:get-problem-data 2025 3))
