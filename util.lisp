(in-package #:advent-of-code-2024.util)

(defun transpose (l)
  (apply #'mapcar #'list l))

(defun histogram (list)
  "Calculate frequency histogram of LIST in the form of a hash table."
  (loop
    with h = (make-hash-table)
    for el in list
    do (incf (gethash el h 0))
    finally (return h)))

(defun split-lines (string)
  "Helper function to split STRING into a list of lines."
  (uiop:split-string string :separator '(#\Newline)))

(defun square (x)
  (* x x))
(setf (fdefinition '^2) #'square)

(defun cube (x)
  (* x x x))
(setf (fdefinition '^3) #'cube)

(defun join-numbers (a b)
  "Combine numbers A and B by juxtaposition."
  (+ (* a (expt 10 (ceiling (log b 10)))) b))

(defun position-in-2d-array (item array)
  "Look for ITEM in a 2-dimensional ARRAY. When found, return its
coordinates in the form (row, column). Otherwise, return NIL."
  (destructuring-bind (d1 d2) (array-dimensions array)
    (loop
      for r from 0 below d1
      do (loop
	   for c from 0 below d2
	   when (equal item (aref array r c))
	     do (return-from position-in-2d-array (list r c))))))

(defun parse-string-into-array (data &key adjustable)
  "Parse DATA string into a 2-dimensional array."
  (let ((char-array (mapcar (alexandria:rcurry #'coerce 'list)
			    (split-lines data))))
    (make-array (list (length char-array) (length (first char-array)))
		:adjustable adjustable
		:initial-contents char-array)))

(defun parse-string-into-list (data)
  "Parse DATA string into a 2-dimensional list."
  (mapcar (alexandria:rcurry #'coerce 'list)
			 (split-lines data)))

(defun merge-plists (plists &key
                              (value-selector #'(lambda (a b) (declare (ignore a)) b))
                              (value-selector-default 0))
  "Merge `plists` into a single plist. Optionally use `value-selector`, a
2-argument function, to select which value will be used in case of key
collision."
  (loop
    with plist = nil
    for (key val) on (apply #'append plists) by #'cddr
    do (setf (getf plist key)
             (funcall value-selector (getf plist key value-selector-default) val))
    finally (return plist)))

#+(or)
(merge-plists '((:a 1 :b 2) (:b 20 :c 30)))
#+(or)
(merge-plists '((:a 1 :b 20 :c 300) (:a 10 :b 200 :c 3) (:a 100 :b 2 :c 30))
              :value-selector #'max)
#+(or)
(merge-plists '((:a 1 :b 20 :c 300) (:a 10 :b 200 :c 3) (:a 100 :b 2 :c 30))
              :value-selector #'min :value-selector-default most-positive-fixnum)

(defun parse-integers (string &key (sep '(#\space #\tab)))
  "Assuming that `string` is a string of space-delimited integers, parse
all of them into a list."
  (loop
    for int-string in (uiop:split-string string :separator sep)
    for int = (parse-integer int-string :junk-allowed t)
    when int
      collect int))

#+(or)
(parse-integers "0 1 2 3 4 5")

#+(or)
(parse-integers "0  1  2  3  4  5")
