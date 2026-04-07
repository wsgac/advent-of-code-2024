(in-package #:soviet-programming-olympiad)

;;;; These are some programming problems from the Soviet Programming
;;;; Olympiad in the 1980s. Taken from the book by A. L. Brudno and
;;;; L. I. Kaplan "Олимпиады по Программированию для Школьников"

;;;;;;;;;;;;;;;;;
;; 80 Olympiad ;;
;;;;;;;;;;;;;;;;;

;; 80.1.1 Напечатать все простые числа, не превосходящие заданное
;; число М. - Print all primes below M.

(defun 80.1.1 (m)
  "Eratosthenes' sieve, essentially."
  (loop
    :with h := (make-hash-table)
    :for d :from 2 :to m
    :unless (gethash d h)
      :do (loop
            :initially (print d)
            :for i = d :then (+ i d)
            :while (<= i m)
            :do (setf (gethash i h) t))))

#+(or)
(80.1.1 20)

;; 80.1.2. Задан массив А(М) из М попарно различных целых
;; чисел. Напечатать все перестановки этих чисел. - All permutations
;; of an M-element array with all elements distinct.

(defun 80.1.2 (arr)
  "Print all permutations of ARR."
  (let ((res (make-array 0 :adjustable t :fill-pointer t)))
    (labels ((swap (arr i j)
               (psetf (aref arr i) (aref arr j)
                      (aref arr j) (aref arr i)))
             (permutations (res arr idx)
               ;; (format t "~%res: ~a arr: ~a idx: ~a" res arr idx)
                  (if (= idx (length arr))
                      (vector-push-extend (copy-seq arr) res)
                      (loop
                        :for i :from idx :below (length arr)
                        :do (swap arr i idx)
                            (permutations res arr (1+ idx))
                            (swap arr i idx)
                        :finally (return res)))))
     (permutations res arr 0))))

#+(or)
(80.1.2 #(1 2 3))

;; 80.1.3. Ввести вещественное число А и натуральное k. Вычислить и
;; напечатать Ак с выполнением следующих условий: операцией возведения
;; в степень пользоваться нельзя; k может оказаться настолько большим,
;; что недопустимо выполнять k умножений.
