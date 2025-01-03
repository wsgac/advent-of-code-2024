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
  (let ((char-list (mapcar (alexandria:rcurry #'coerce 'list)
                                        ; (lambda (line) (coerce line 'list))
			    (split-lines data))))
    (make-array (list (length char-list) (length (first char-list)))
		:adjustable adjustable
		:initial-contents char-list)))

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

#+(or)
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

(defun parse-integers (string &key
                                (sep '(#\space)))
  "Assuming that `string` is a string of space-delimited integers, parse
all of them into a list."
  (loop
    for int-string in (uiop:split-string string :separator sep)
    for maybe-int = (parse-integer int-string :junk-allowed t)
    when maybe-int
      collect maybe-int))

#+(or)
(parse-integers "0 1 2 3 4 5")

#+(or)
(parse-integers "0  1  2  3  4  5")

#+(or)
(defun choose-combinations-not-replacing (set k)
  (labels ((choose-tail (set k acc)
             (when (>= (length set) k)
               (if (zerop k)
                   acc
                   (let ((acc-w/o-first (choose-tail (rest set) k acc))
                         (acc-w/-first
                           (choose-tail (rest set) (1- k)
                                        (or (mapcar (a:curry #'cons (first set)) acc)
                                            (list (list (first set)))))))
                     (append acc-w/-first
                             acc-w/o-first))))))
    (choose-tail set k nil)))

(defun choose-combinations-not-replacing (set k)
  (when (<= k (length set))
    (cond
      ((zerop k) nil)
      ((= k 1) (mapcar #'list set))
      (t (append (choose-combinations-not-replacing (rest set) k)
                 (loop
                   for sub in (choose-combinations-not-replacing (rest set) (1- k))
                   collect (cons (first set) sub)))))))

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

(defun digits->number (digits)
  (reduce (lambda (n d) (+ (* 10 n) d)) digits :initial-value 0))

#+(or)
(digits->number '(1 2 3 4))

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

(defmacro array-loop ((array (&rest indices) &key (item nil itemp)) &body body)
  (a:once-only (array)
    `(progn
       (assert (= (array-rank ,array) ,(length indices))
               () "ARRAY-LOOP: array rank: ~d variables provided: ~d"
               (array-rank ,array) ,(length indices))
       ,(%array-loop array 0 indices (if itemp item (gensym)) body))))


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

;; (defun bron-kerbosch-with-pivot (r p x graph cliques)
;;   (if (and (endp p) (endp x))
;;       (values (cons r cliques) r p x)
;;       (progn
;;         (loop
;;          ;; for v in (set-difference p (gethash pivot graph))
;;         for v in p
;;          for nv = (gethash v graph)
;;          do (multiple-value-setq (cliques r p x)
;;               (bron-kerbosch-with-pivot (union r (list v))
;;                                         (intersection p (gethash v graph))
;;                                         (intersection x (gethash v graph))
;;                                         graph cliques))
;;          do (a:removef p v)
;;          do (pushnew v x))
;;         (values cliques r p x))))

;; Borrowed from https://github.com/fcbr/graph-algorithms/blob/master/maximal-cliques.lisp
(defun bron-kerbosch (R P X neighbors-fn visitor-fn)
  "The basic form of the Bron–Kerbosch algorithm is a recursive
backtracking algorithm that searches for all maximal cliques in a
given graph G. More generally, given three disjoint sets of vertices
R, P, and X, it finds the maximal cliques that include all of the
vertices in R, some of the vertices in P, and none of the vertices in
X. In each call to the algorithm, P and X are disjoint sets whose
union consists of those vertices that form cliques when added to R. In
other words, P ∪ X is the set of vertices which are joined to every
element of R. When P and X are both empty there are no further
elements that can be added to R, so R is a maximal clique and the
algorithm outputs R."
  (when (and (a:emptyp P) (a:emptyp X))
    (funcall visitor-fn R))
  (dolist (v P)
    (let ((nv (funcall neighbors-fn v)))
      (bron-kerbosch
       (union R (list v))
       (intersection P nv)
       (intersection X nv)
       neighbors-fn visitor-fn)
      (a:removef P v)
      (push v X))))


(defun compose-n (f n)
  (apply #'a:compose (make-list n :initial-element f)))

(defun vector-push-n (vec item n)
  (loop
    repeat n
    do (vector-push-extend item vec)
    finally (return vec)))
