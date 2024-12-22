(in-package #:advent-of-code-2024.day21)

(defun parse-digit-or-a (ch)
  (or (digit-char-p ch)
      (when (char= #\A ch) 'a)))

(defun parse-input (input)
  (loop
    for row in (util:split-lines input)
    collect (map 'list #'parse-digit-or-a row)))

(defun complexity (code)
  (* (util:digits->number (butlast code))
     (length (shortest-sequence code))))

(defun remove-adjacent-duplicates (p)
  (loop
    for (a b) on p
    unless (equal a b)
      collect a))

(defun shortest-sequence (code)
  (flet ((to-cursor-coord (p)
           (loop
             for (a b) on (remove-adjacent-duplicates p)
             while b
             collect (list (- (first b) (first a))
                           (- (second b) (second a))))))
   (let* ((code-coord (mapcar #'numeric-kbd-sym-to-coord (cons 'a code)))
          (paths (mapcar #'to-cursor-coord (numeric-kbd-code-paths code-coord))))
     paths
     #+(or)
     (loop
       for path in paths
       collect (cursor->cursor
                (cursor->cursor
                 (numeric->cursor path)))
         into sequences
       finally (first (sort sequences #'< :key #'length))))))

(defun shortest-num-path (p1 p2)
  (flet ((to-cursor-coord (p)
           (loop
             for (a b) on p
             while b
             collect (list (- (first b) (first a))
                           (- (second b) (second a)))))
         (path-num-len (path)
              (length (cursor->cursor (mapcar #'coord-to-cursor path)))))
   (let ((paths (mapcar #'to-cursor-coord (numeric-kbd-paths p1 p2))))
     (first (sort paths #'< :key #'path-num-len)))))

(defun numeric->cursor (path)
  (loop
    for (p1 p2) on path
    when p2
      append (append (mapcar #'coord-to-cursor (shortest-num-path p1 p2))
                     (list #\A))
        into cursors
    finally (return cursors)))


(defun cursor->cursor (code)
  (loop
    for (p1 p2) on (cons #\A code)
    when p2
      append (mapcar #'coord-to-cursor
                     (cursor-kbd-path p1 p2))
        into cursors
    when p2
      collect #\A into cursors
    finally (return cursors)))

;; +---+---+---+
;; | 7 | 8 | 9 |
;; +---+---+---+
;; | 4 | 5 | 6 |
;; +---+---+---+
;; | 1 | 2 | 3 |
;; +---+---+---+
;;     | 0 | A |
;;     +---+---+

(defun numeric-kbd-sym-to-coord (sym)
  (cond
    ((eql sym 'a) '(3 2))
    ((zerop sym) '(3 1))
    ((integerp sym)
     (list (- 3 (ceiling (/ sym 3)))
           (mod (1- sym) 3)))))

#+(or)
(mapcar #'numeric-kbd-sym-to-coord '(A 0 1 2 3 4 5 6 7 8 9))

(defun numeric-kbd-code-paths (code)
  (if (< (length code) 2)
      nil
      (let ((tail-paths (numeric-kbd-code-paths (cdr code))))
        (if tail-paths
            (loop
              for path in (numeric-kbd-paths (first code) (second code))
              append (loop
                       for tail-path in tail-paths
                       collect (append path (list '(0 0)) tail-path)))
            (numeric-kbd-paths (first code) (second code))))))

(defun numeric-kbd-paths (from to)
  (destructuring-bind (from-row from-col) from
    (destructuring-bind (to-row to-col) to
      (cond
        ((equal from '(3 0)) nil)
        ((equal from to) (list (list to)))
        (t (let* ((dr (signum (- to-row from-row)))
                  (dc (signum (- to-col from-col)))
                  (paths (append (unless (zerop dr) (numeric-kbd-paths
                                                     `(,(+ from-row dr) ,from-col) to))
                                 (unless (zerop dc) (numeric-kbd-paths
                                                     `(,from-row ,(+ from-col dc)) to)))))
             (loop
               for path in paths
               collect (cons from path))))))))

#+(or)
(numeric-kbd-paths (numeric-kbd-sym-to-coord 7) (numeric-kbd-sym-to-coord 0))
#+(or)
(numeric-kbd-paths (numeric-kbd-sym-to-coord 0) (numeric-kbd-sym-to-coord 7))
#+(or)
(numeric-kbd-paths (numeric-kbd-sym-to-coord 7) (numeric-kbd-sym-to-coord 'a))
#+(or)
(numeric-kbd-paths (numeric-kbd-sym-to-coord 'a) (numeric-kbd-sym-to-coord 7))

(defun coord-to-cursor (coord)
  (cond
    ((equal coord '(0 -1)) #\<)
    ((equal coord '(0 1)) #\>)
    ((equal coord '(1 0)) #\v)
    ((equal coord '(-1 0)) #\^)
    ((equal coord '(0 0)) #\A)))

;;     +---+---+
;;     | ^ | A |
;; +---+---+---+
;; | < | v | > |
;; +---+---+---+

(defun cursor-kbd-sym-to-coord (sym)
  (ecase sym
    (#\< '(1 0))
    (#\v '(1 1))
    (#\> '(1 2))
    (#\^ '(0 1))
    (#\A '(0 2))))

(defun cursor-kbd-path (from to)
  (destructuring-bind (from-row from-col) (cursor-kbd-sym-to-coord from)
    (destructuring-bind (to-row to-col) (cursor-kbd-sym-to-coord to)
      (let ((h (if (< from-col to-col)
                   (loop for c from from-col below to-col collect '(0 1))
                   (loop for c from to-col below from-col collect '(0 -1)))))
        (if (< from-row to-row)
            (append (loop for r from from-row below to-row collect '(1 0)) h)
            (append h (loop for r from to-row below from-row collect '(-1 0))))))))

#+(or)
(cursor-kbd-path #\< #\A)
#+(or)
(cursor-kbd-path #\A #\<)

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    for code in (parse-input input)
    sum (complexity code)))




(defparameter *input-part-1-test*
  "029A
980A
179A
456A
379A")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  "340A
149A
582A
780A
463A")
