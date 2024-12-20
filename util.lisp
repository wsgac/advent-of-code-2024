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
#+(or)
(merge-plists '((:a 1 :b 2) (:b 20 :c 30)) :value-selector #'+)

(defun parse-integers (string &key
                                (sep '(#\space #\tab)))
  "Assuming that `string` is a string of space-delimited integers, parse
all of them into a list."
  (declare (ignorable sep))
  (loop
    for s = 0 then offset
    for (n offset) = (multiple-value-list
                      (parse-integer string :junk-allowed t :start s))
    while n
    collect n))

#+(or)
(parse-integers "0 1 2 3 4 5")

#+(or)
(parse-integers "0  1  2  3  4  5")

(defun choose-combinations-replacing (set k)
  (labels ((choose-tail (set k acc)
             (if (zerop k)
                 acc
                 (choose-tail set (1- k)
                              (mapcan (lambda (s)
                                        (if acc
                                            (mapcar (lambda (l)
                                                      (cons s l))
                                                    acc)
                                            (list (list s))))
                                      set)))))
    (choose-tail set k nil)))

(defun concat-numbers (a b)
  (if (zerop b) (* 10 a)
      (+ (* a (expt 10 (floor (1+ (log b 10))))) b)))

;; (defun number->digits (n)
;;   (loop
;;     for (nn r) = (multiple-value-list (truncate n 10))
;;       then (multiple-value-list (truncate nn 10))
;;     collect r into digits
;;     while (plusp nn)
;;     finally (return (or (nreverse digits) '(0)))))

(defun number->digits (n)
  (labels ((n->d (n acc)
             (if (zerop n)
                 (or acc '(0))
                 (multiple-value-bind (n r)
                     (truncate n 10)
                   (n->d n (cons r acc))))))
    (n->d n nil)))

(defun char-arr->num-arr (arr)
  (let ((arr2 (make-array (array-dimensions arr)))
        (rows (array-dimension arr 0))
        (cols (array-dimension arr 1)))
    (loop
      for row from 0 below rows
      do (loop
           for col from 0 below cols
           do (setf (aref arr2 row col)
                    (digit-char-p (aref arr row col)))))
    arr2))

(defun extract-integers (string)
  (mapcar #'parse-integer
          (ppcre:all-matches-as-strings "-?[0-9]+" string)))

(defun %array-loop (array dim indices item body)
  `(loop
     for ,(nth dim indices) from 0 below (array-dimension ,array ,dim)
     ,@(if (= dim (1- (length indices)))
           `(for ,item = (aref ,array ,@indices)
                 do (progn ,@body))
           `(do ,(%array-loop array (1+ dim) indices item body)))))

(defmacro array-loop ((array (&rest indices) &key item) &body body)
  `(a:with-gensyms ,(list* item indices)
     ,(%array-loop array 0 indices item `,body)))



;; (array-loop (arr (r c) :item el)
;;   (format t "Row: ~a Col: ~a Item: ~a~%" r c el))

;; Priority Queue

(defclass priority-queue ()
  ((queue
    :initarg :list
    :initform nil
    :accessor queue)
   (sorted
    :initform nil
    :accessor sorted)
   (cmp
    :initarg :cmp
    :accessor cmp)
   (key
    :initarg :key
    :accessor key)))

(defmethod pq-push ((pq priority-queue) item)
  (prog1 (push item (queue pq))
    (setf (sorted pq) nil)))

(defmethod pq-pop ((pq priority-queue))
  (with-slots (queue cmp key sorted) pq
    (unless sorted
      (setf queue (sort queue cmp :key key)
            sorted t))
    (pop queue)))

(defmethod pq-empty? ((pq priority-queue))
  (endp (list pq)))

(defun vec+ (v1 v2)
  (mapcar #'+ v1 v2))

(defun manhattan (p1 p2)
  "Manhattan metric between `p1` and `p2`."
  (reduce #'+ (mapcar (lambda (a b) (abs (- a b))) p1 p2)))
