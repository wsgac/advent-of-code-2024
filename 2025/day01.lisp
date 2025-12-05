(in-package #:advent-of-code-2025.day01)

(defun parse-input (input)
  (loop
    :for row :in (util:split-lines input)
    :for dir := (a:make-keyword (subseq row 0 1))
    :for mag := (parse-integer (subseq row 1))
    :if (eql dir :l)
      :collect (- mag)
    :else
      :collect mag))


(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :for mag :in (parse-input input)
    :for pos := (mod (+ 50 mag) 100) :then (mod (+ pos mag) 100)
    :count (zerop pos)))

#+(or)
(defun chop (magnitudes)
  (labels ((%chop (mag acc)
             (cond
               ((< mag -100)
                (%chop (+ mag 100) (cons -100 acc)))
               ((> mag 100)
                (%chop (- mag 100) (cons 100 acc)))
               (t (cons mag acc)))))
    (loop
      :for mag :in magnitudes
      :append (%chop mag nil))))

(defun problem2 (&key (input *input-part-1-test*))
  (loop
    :with zeros := 0
    :with pos := 50
    :for mag :in (parse-input input)
    :if (minusp mag)
      :do (progn
            (when (and (plusp pos) (<= pos (- mag)))
              (incf zeros))
            (incf pos mag)
            (when (<= pos 0)
              (incf zeros (truncate (- pos) 100))))
    :else
      :do (progn
            (incf pos mag)
            (when (>= pos 100)
              (incf zeros (truncate pos 100))))
    :do (setf pos (mod pos 100))
    :finally (return zeros)))

(defparameter *input-part-1-test*
  "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (util:get-problem-data 2025 1))
