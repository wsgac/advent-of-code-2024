(in-package #:soviet-programming-olympiad)

;;;; These are some programming problems from the Soviet Programming
;;;; Olympiad in the 1980s. Taken from the book by A. L. Brudno and
;;;; L. I. Kaplan "Олимпиады по Программированию для Школьников"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Олимпиада 80 - 80 Olympiad ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; напечатать А^к с выполнением следующих условий: операцией возведения
;; в степень пользоваться нельзя; k может оказаться настолько большим,
;; что недопустимо выполнять k умножений.

(defun 80.1.3 (a k)
  (check-type a real)
  (check-type k integer)
  (loop
    :with res := 1
    :for kk := k :then (ash kk -1)
    :while (plusp kk)
    :when (= 1 (logand 1 kk))
      :do (setf res (* res a))
    :do (setf res (* res a))
    :finally (return res)))

#+(or)
(80.1.3 3 5)
#+(or)
(80.1.3 pi 5)

;; 80.1.4. В написанном выражении ((((1?2)?3)?4)?5)?6 вместо каждого
;; знака ? вставить знак одной из четырех арифметических операций +,
;; —, ✕, / так, чтобы результат вычислений равнялся 35 (при делении
;; дробная часть в частном отбрасывается). Достаточно найти одно
;; решение.

(defun generate-combinations ()
  )

(defun 80.1.4 ()
  (let ((combinations (util:choose-combinations-replacing (list #'+ #'- #'* #'/) 5))
        (arguments (a:iota 6 :start 1)))
    (find-if (lambda (c) (= 35 (util:interleave-functions c arguments))) combinations)))

;; 80.2Л. Дан двумерный целочисленный массив A(2,15). Известно, что
;; среди его элементов два и только два равны между собой. Напечатать
;; их индексы.

;; 80.2.2. Можно ли заданное натуральное число М представить в виде
;; суммы двух квадратов натуральных чисел? Написать программу решения
;; этой задачи.

;; 80.2.3. Даны натуральное число М и целочисленный массив
;; А(М). Сосчитать и напечатать, сколько различных чисел в этом
;; массиве. Например, в массиве 5, 7,5 различных чисел два (5 и 7).

;; 80.3.1. Составить программу вывода всех трехзначных десятичных
;; чисел, сумма цифр которых равна данному целому числу.

;; 80.3.2. Целое неотрицательное число М задано массивом своих
;; двоичных цифр a_0, a_1, ..., a_{n-1}: М = a_{n-i}2^{n-1} +
;; а_{n-2}2^{n-2} + ... + а_{1}2+а_0, где a_i = 0 или а_i = 1 (i = 0,
;; ..., n—1). Напечатать массив двоичных цифр числа М + 1.

;; 80.3.3. В массиве X(M,N) все числа различны. В каждой строке
;; выбирается минимальный элемент,затем среди этих чисел выбирается
;; максимальное. Напечатать номер строки массива X, в которой
;; расположено выбранное число.

;; 80.3.4. В массиве X(N) каждый элемент равен 0, 1 или 2. Переставить
;; элементы массива так, чтобы сначала располагались все нули, затем
;; все единицы и,наконец, все двойки (дополнительного массива не
;; заводить).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Олимпиада 81 - 81 Olympiad ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
