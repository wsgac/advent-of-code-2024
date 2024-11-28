(in-package #:advent-of-code-2023.day06)

(defun parse-input (input)
  (destructuring-bind (time-str dist-str)
      (util:split-lines input)
    (mapcar (lambda (time dist)
              `(:time ,time
                :dist ,dist))
            (parse-integers time-str)
            (parse-integers dist-str))))

(defun count-winning-configurations (&key time dist)
  (loop
    for pressed from 1 below time
    for dist-traveled = (* pressed (- time pressed))
    when (> dist-traveled dist)
      count 1))

(defun problem-1 (&key (input *input-part-1-test*))
  (reduce (lambda (acc x)
            (* acc (apply #'count-winning-configurations x)))
          (parse-input input) :initial-value 1))

(defun parse-input-2 (input)
  (destructuring-bind (time-str dist-str)
      (util:split-lines input)
    `(:time ,(parse-integer (apply #'str:concat (rest (str:split " " time-str)))
                            :junk-allowed t)
      :dist ,(parse-integer (apply #'str:concat (rest (str:split " " dist-str)))
                            :junk-allowed t))))

(defun problem-2 (&key (input *input-part-2-test*))
  "Count winning combinations of charging and racing times for
`input`. Perform a brute-force search."
  (apply #'count-winning-configurations (parse-input-2 input)))

(defun problem-2-quadratic (&key (input *input-part-2-test*))
  "Count winning combinations of charging and racing times for
`input`. Find winning range by solving a quadratic equation."
  (destructuring-bind (time-str dist-str)
      (util:split-lines input)
    (let* ((time (parse-integer (apply #'str:concat (rest (str:split " " time-str)))
                                :junk-allowed t))
           (dist (parse-integer (apply #'str:concat (rest (str:split " " dist-str)))
                                :junk-allowed t))
           (delta (- (util:^2 time) (* 4 dist)))
           (t1 (floor (/ (+ time (isqrt delta)) 2)))
           (t2 (floor (/ (- time (isqrt delta)) 2))))
      (truncate (abs (- t1 t2))))))

(defparameter *input-part-1-test*
  "Time:      7  15   30
Distance:  9  40  200")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  "Time:        54     94     65     92
Distance:   302   1476   1029   1404")
