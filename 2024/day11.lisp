(in-package #:advent-of-code-2024.day11)

(defun parse-input (input)
  (util:parse-integers input))

(defun blink-stone (stone)
  (let ((digits (util:number->digits stone)))
   (cond
     ((zerop stone) (list 1))
     ((evenp (length digits))
      (multiple-value-list (truncate stone
                                     (expt 10 (ash (length digits) -1)))))
     (t (list (* 2024 stone))))))

(defun blink (stones)
  (loop
    for stone in stones
    append (blink-stone stone)))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    repeat 26
    for stones = (parse-input input) then (blink stones)
    ;; do (print stones)
    finally (return (length stones))))

(defun stone-after-blinks (stone n h)
  (let* ((digits (length (util:number->digits stone)))
         (result (gethash (list n stone) h)))
    (if result
        result
        (setf (gethash (list n stone) h) 
              (cond
                ((zerop n) 1)
                ((zerop stone)
                 (stone-after-blinks 1 (1- n) h))
                ((evenp digits)
                 (multiple-value-bind (left right)
                     (truncate stone (expt 10 (ash digits -1)))
                   (+ (stone-after-blinks left (1- n) h)
                      (stone-after-blinks right (1- n) h))))
                (t (stone-after-blinks (* 2024 stone) (1- n) h)))))))

(defun problem-2 (&key (input *input-part-2-test*))
  (loop
    with h = (make-hash-table :test #'equal)
    for stone in (parse-input input)
    for score = (stone-after-blinks stone 75 h)
    sum score))

(defparameter *input-part-1-test*
  "125 17")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  "6 11 33023 4134 564 0 8922422 688775")
