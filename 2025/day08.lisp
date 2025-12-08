(in-package #:advent-of-code-2025.day08)

(defun parse-input (input)
  (loop
    :for row :in (u:split-lines input)
    :collect (u:parse-integers row :sep '(#\,))))

(defparameter *dist-cache* (make-hash-table :test #'equal))

(defun dist (p1 p2)
  (a:if-let ((d (gethash (list p1 p2) *dist-cache*)))
    d
    (setf (gethash (list p1 p2) *dist-cache*)
          (sqrt (reduce #'+ (mapcar #'u:square (mapcar #'- p1 p2)))))))

(defun pairs-ascending (points &key (keep))
  (loop
    :for (p1 . p1t) :on points
    :append (loop
              :for p2 :in p1t
              :collect (cons (list p1 p2) (dist p1 p2)))
      :into pairs
    :finally (return (if keep
                         (subseq (sort pairs #'< :key #'cdr) 0 keep)
                         (sort pairs #'< :key #'cdr)))))

(defun build-graph (pairs)
  (loop
    :with h := (make-hash-table :test #'equal)
    :for ((p1 p2) . _) :in pairs
    :do (pushnew p2 (gethash p1 h) :test #'equal)
    :do (pushnew p1 (gethash p2 h) :test #'equal)
    :finally (return h)))

(defun build-circuits (points pairs cutoff)
  (let ((pp (1- (length points)))
        circuits
        prod
        (connections (make-hash-table :test #'equal)))
    (labels ((follow (x)
               (a:if-let ((el (gethash x connections)))
                 (follow el)
                 x)))
      (loop
        :with conn := 0
        :for i :from 0
        :for ((p1 p2) . _) :in pairs
        :when (= i cutoff)
          :do (setf circuits
                    (loop
                      :with h := (make-hash-table :test #'equal)
                      :for p :in points
                      :do (incf (gethash (follow p) h 0))
                      :finally (return (subseq (sort (a:hash-table-values h) #'>) 0 3))))
        :unless (equal (follow p1) (follow p2))
          :do (setf (gethash (follow p1) connections)
                    (follow p2))
          :and :do (incf conn)
          :and :do (when (= conn pp)
                     (setf prod (* (first p1) (first p2)))))
      (list (reduce #'* circuits) prod))))

(defun problem1 (&key (input *input-part-1-test*) (keep 10))
  (let* ((points (parse-input input))
         (pairs (pairs-ascending points)))
    (first (build-circuits points pairs keep))))

(defun problem2 (&key (input *input-part-1-test*))
  (let* ((points (parse-input input))
         (pairs (pairs-ascending points)))
    (second (build-circuits points pairs 10))))

(defparameter *input-part-1-test*
  "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2025 8))
