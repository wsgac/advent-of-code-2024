(in-package #:advent-of-code.util)

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

(defun split-string-on-indices (string indices)
  (declare (string string)
           (list indices))
  (loop
    :for start := 0 :then end
    :for end :in (append indices (list nil))
    :collect (subseq string start end)))

(defun parse-string-into-list (data)
  "Parse DATA string into a 2-dimensional list."
  (mapcar (alexandria:rcurry #'coerce 'list)
	  (split-lines data)))

(defun merge-plists (plists &key
                              (value-selector #'(lambda (a b) (declare (ignore a)) b)))
  "Merge `plists` into a single plist. Optionally use `value-selector`, a
2-argument function, to select which value will be used in case of key
collision. That defaults to a newest value selector."
  (loop
    with result = nil
    for plist in plists
    do (loop
         for (key val) on plist by #'cddr
         do (setf (getf result key)
                  (a:if-let (curr (getf result key))
                    (funcall value-selector curr val) val)))
    finally (return result)))

#+(or)
(merge-plists '((:a 1 :b 2) (:b 20 :c 30)))
#+(or)

(merge-plists '((:a 1 :b 20 :c 300) (:a 10 :b 200 :c 3) (:a 100 :b 2 :c 30))
              :value-selector #'max)
#+(or)
(merge-plists '((:a 1 :b 20 :c 300) (:a 10 :b 200 :c 3) (:a 100 :b 2 :c 30))
              :value-selector #'min)
#+(or)
(merge-plists '((:a 1 :b 2) (:b 20 :c 30)) :value-selector #'+)

(defun list->groups (list n)
  "Divide `list` into groups of (at most) `n` elements."
  (loop
    :for i :from 0
    :with group := nil
    :for el :in list
    :do (push el group)
    :when (and (plusp i) (zerop (mod (1+ i) n)))
      :collect (nreverse group) :into groups
      :and :do (setf group nil)
    :finally (return (append groups (when group (list group))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings & Characters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-of-chars (lst)
  (every #'characterp lst))

(deftype character-list ()
  '(and list (satisfies list-of-chars)))

;;;;;;;;;;;;;;;;;;;
;; Combinatorics ;;
;;;;;;;;;;;;;;;;;;;

(defun factorial (n)
  "Tail-recursive factorial function of `n`"
  (labels ((tail (n acc)
             (if (zerop n)
                 acc
                 (tail (1- n) (* n acc)))))
    (tail n 1)))

#+(or)
(factorial 10)

(defun choose (n k)
  "Efficient implementation of the binomial coefficient ($\binom{n}{k}$)"
  (loop
    with result = 1
    for i from n downto (1+ (max k (- n k)))
    do (setf result (* result i))
    finally (return (/ result (factorial (min k (- n k)))))))

#+(or)
(choose-naive 52 5)

;; Compare with naive implementation
#+(or)
(flet ((choose-naive (n k)
         (/ (factorial n)
            (* (factorial (- n k)) (factorial k)))))
  (princ "Naive: ")
  (time (loop repeat 1000000 do (choose-naive 52 5)))
  (princ "Efficient: ")
  (time (loop repeat 1000000 do (choose 52 5))))

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
  "Generate `k`-element combinations of elements from `set`, without
replacing chosen elements. This should produce $\binom{|set|}{k}$
or `(choose (length set) k)` items."
  (when (<= k (length set))
    (cond
      ((zerop k) nil)
      ((= k 1) (mapcar #'list set))
      (t (append (choose-combinations-not-replacing (rest set) k)
                 (loop
                   for sub in (choose-combinations-not-replacing (rest set) (1- k))
                   collect (cons (first set) sub)))))))

#+(or)
(choose-combinations-not-replacing '(a b c d e) 3)

(defun choose-combinations-replacing (set k)
  "Generate `k`-element combinations of elements from `set`, replacing
chosen elements. This should produce $|set|^k$ items."
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

#+(or)
(choose-combinations-replacing '(a b c d e) 3)

;;;;;;;;;;;;;;;;;;;;;;
;; Digits & Numbers ;;
;;;;;;;;;;;;;;;;;;;;;;

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
(parse-integers "0-1-2-3-4-5" :sep '(#\-))

(defun parse-ranges (string)
  "Parse `string` looking for integer ranges of the form a-b."
  (let (pairs)
   (re:do-register-groups (from to)
       ("(\\d+)-(\\d+)" string)
     (push (cons (parse-integer from) (parse-integer to)) pairs))
    pairs))

(defun concat-numbers (a b)
  (if (zerop b) (* 10 a)
      (+ (* a (expt 10 (floor (1+ (log b 10))))) b)))

#+(or)
(concat-numbers 12345 67890)

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

#+(or)
(number->digits 12345)

#+(or)
(number->digits 0)

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

(defun square (x)
  (* x x))
(setf (fdefinition '^2) #'square)

(defun cube (x)
  (* x x x))
(setf (fdefinition '^3) #'cube)

(defun join-numbers (a b)
  "Combine numbers A and B by juxtaposition."
  (+ (* a (expt 10 (ceiling (log b 10)))) b))

;;;;;;;;;;;;
;; Arrays ;;
;;;;;;;;;;;;

(defun aref* (array indices)
  "Lookup the contents of `array` under indices contained in
`indices`. The length of `indices` must be the same as the rank of
`array`."
  (assert (= (length indices) (array-rank array)))
  (apply #'aref array indices))

(defun (setf aref*) (val array indices)
  "Lookup the contents of `array` under indices contained in
`indices`. The length of `indices` must be the same as the rank of
`array`."
  (assert (= (length indices) (array-rank array)))
  (setf (apply #'aref array indices) val))

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

(defun %array-loop (array dim indices item body)
  `(loop
     for ,(nth dim indices) from 0 below (array-dimension ,array ,dim)
     ,@(if (= dim (1- (length indices)))
           `(for ,item = (aref ,array ,@indices)
                 do (progn ,@body))
           `(do ,(%array-loop array (1+ dim) indices item body)))))

(defmacro array-loop ((array (&rest indices) &key (item nil itemp)) &body body)
  "Abstract away the nested loop when iterating through an n-dimensional
`array`. Bind identifiers listed in `indices` to consecutive
dimensional indices of `array`. When provided, bind consecutive item
values to `item`. For each such iteration execute `body`."
  (a:once-only (array)
    `(progn
       (assert (= (array-rank ,array) ,(length indices))
               () "ARRAY-LOOP: array rank: ~d variables provided: ~d"
               (array-rank ,array) ,(length indices))
       ,(%array-loop array 0 indices (if itemp item (gensym)) body))))

#+(or)
(let ((arr (make-array '(3 3 3)
                       :initial-contents
                       (loop for i below 3
                             collect (loop for j below 3
                                           collect (loop for k below 3
                                                         collect (+ (* 9 i) (* 3 j) k)))))))
  (array-loop (arr (d1 d2 d3) :item el)
    (format t "Dimension 1: ~a Dimension 2: ~a Dimension 3: ~a Item: ~a~%" d1 d2 d3 el)))

;; Priority Queue - BEGIN

(defclass priority-queue-list ()
  ((queue
    :initarg :queue
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

(defmethod pq-push ((pq priority-queue-list) item)
  (prog1 (push item (queue pq))
    (setf (sorted pq) nil)))

(defmethod pq-pop ((pq priority-queue-list))
  (with-slots (queue cmp key sorted) pq
    (unless sorted
      (setf queue (sort queue cmp :key key)
            sorted t))
    (pop queue)))

(defmethod pq-empty? ((pq priority-queue-list))
  (endp (queue pq)))

#+(or)
(let ((q (make-instance 'priority-queue-list :cmp #'> :key #'second))
      (items '((a 1) (b 2) (c 3) (d 4) (e 5))))
  (dolist (i items)
    (pq-push q i))
  (loop
    collect (pq-pop q)
    until (pq-empty? q)))

(defclass priority-queue ()
  ((heap
    :initarg :heap
    :initform nil
    :accessor heap)
   (vec-size
    :initarg :vec-size)
   (cmp
    :initarg :cmp
    :accessor cmp)
   (key
    :initarg :key
    :accessor key)))

(defmethod initialize-instance :after ((pq priority-queue) &key &allow-other-keys)
  (with-slots (heap cmp key vec-size) pq
    (let ((h (serapeum:make-heap :key key :test cmp :size vec-size)))
      (unless (endp heap)
        (dolist (item heap)
          (serapeum:heap-insert h item)))
      (setf heap h))))

(defmethod pq-push ((pq priority-queue) item)
  (with-slots (heap) pq
    (serapeum:heap-insert heap item)))

(defmethod pq-pop ((pq priority-queue))
  (with-slots (heap) pq
    (serapeum:heap-extract-maximum heap)))

(defmethod pq-empty? ((pq priority-queue))
  (with-slots (heap) pq
    (a:emptyp (serapeum::heap-vector heap))))

#+(or)
(let ((q (make-instance 'priority-queue :cmp #'> :key #'second))
      (items '((a 1) (b 2) (c 3) (d 4) (e 5))))
  (dolist (i (a:shuffle items))
    (pq-push q i))
  (loop
    collect (pq-pop q)
    until (pq-empty? q)))

#+(or)
(let ((q (make-instance 'priority-queue :cmp #'< :key #'second))
      (items '((a 1) (b 2) (c 3) (d 4) (e 5))))
  (dolist (i (a:shuffle items))
    (pq-push q i))
  (loop
    collect (pq-pop q)
    until (pq-empty? q)))

#+(or)
(let ((ql (make-instance 'priority-queue-list :cmp #'> :key #'second))
      (qh (make-instance 'priority-queue :cmp #'> :key #'second :vec-size 100000)))
  ;; Fill queues
  (loop
    for i from 0 to 100000
    for s = (gensym)
    do (pq-push ql (list s i))
    do (pq-push qh (list s i)))
  ;; Time processing of list-based priority queue
  (princ "list-based")
  (time
   (loop
     do (pq-pop ql)
     until (pq-empty? ql)))
  ;; Time processing of heap-based priority queue
  (princ "heap-based")
  (time
   (loop
     do (pq-pop qh)
     until (pq-empty? qh))))

;; Priority Queue - END

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
  "Compose `n` instaces of function `f`."
  (apply #'a:compose (make-list n :initial-element f)))

#+(or)
(funcall (compose-n #'square 3) 2)

(defun vector-push-n (vec item n)
  "Push `n` instances of `item` to vector `vec`. This requires `vec` to
have a fill pointer and be adjustable."
  (loop
    repeat n
    do (vector-push-extend item vec)
    finally (return vec)))

#+(or)
(let ((v (make-array '(3) :initial-contents '(1 2 3) :fill-pointer t :adjustable t)))
  (vector-push-n v 10 5)
  v)

;;;;;;;;;;;;;
;; Streams ;;
;;;;;;;;;;;;;

(defun %with-output-to-multiple% (stream-var files file-streams body)
  (if (null files)
      `(let ((,stream-var (make-broadcast-stream ,@file-streams)))
         (progn ,@body))
      (let ((s (gensym)))
        `(with-open-file (,s ,(first files) :direction :output)
           ,(%with-output-to-multiple% stream-var (rest files) (cons s file-streams) body)))))

(defmacro with-output-to-multiple (stream-var (&rest files) &body body)
  "Create a broadcast stream connected to the output streams of `files`
and bind it to `stream-var`. Execute `body` in the context of those
bindings."
  (%with-output-to-multiple% stream-var files nil body))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files & Filesystems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun file-at-once (filespec &rest open-args)
  (with-open-stream (stream (apply #'open filespec
                                   open-args))
    (let* ((buffer
             (make-array (file-length stream)
                         :element-type
                         (stream-element-type stream)
                         :fill-pointer t))
           (position (read-sequence buffer stream)))
      (setf (fill-pointer buffer) position)
      buffer)))

(defun (setf file-at-once) (file-contents filespec)
  (with-open-stream (stream (open filespec :direction :output :if-exists :supersede))
    (write-sequence file-contents stream)))

;;;;;;;;;;;;;;;;;
;; Odds & Ends ;;
;;;;;;;;;;;;;;;;;

(defun country->flag (country)
  "Convert a two-letter `country` code into a Unicode flag."
  (flet ((regional-indicator (c)
           (assert (char<= #\a (char-downcase c) #\z)
                   nil "Character needs to be a Latin letter. Got ~c instead." c)
           (code-char (+ (char-code #\🇦) ;; Alternatively use Unicode 0x1f1e6
                         (- (char-code (char-downcase c)) (char-code #\a))))))
    (map 'string #'regional-indicator country)))

#+(or)
(country->flag "pl")

#+(or)
(country->flag "UA")

(defvar *alphabet* (map 'string (lambda (c) (code-char (+ c (char-code #\a)))) (a:iota 26)))

(defun all-your-flags-are-belong-to-us ()
  (flet ((regional-indicator (c)
           (assert (char<= #\a (char-downcase c) #\z)
                   nil "Character needs to be a Latin letter. Got ~c instead." c)
           (code-char (+ (char-code #\🇦) ;; Alternatively use Unicode 0x1f1e6
                         (- (char-code (char-downcase c)) (char-code #\a))))))
   (with-output-to-string (s)
     (format s "~%  ~{~a​~}~%" (mapcar #'regional-indicator (coerce *alphabet* 'list)))
     (loop
       for c1 across *alphabet*
       do (format s "~a​" (regional-indicator c1))
       do (loop
            for c2 across *alphabet*
            do (format s "~a" (country->flag (format nil "~c~c" c1 c2))))
       do (terpri s)))))
