(in-package #:sudoku)



(defun cross (a b)
  "Compute cartesian product between elements of `a` and `b`. `a` and `b`
can be of type CHARACTER, STRING or list of CHARACTERs."
  (declare ((or character string character-list) a)
           ((or character string character-list) b))
  (let ((as (typecase a
              (character (a:ensure-list a))
              (string (coerce a 'list))
              (t a)))
        (bs (typecase b
              (character (a:ensure-list b))
              (string (coerce b 'list))
              (t b))))
    (loop
      :for a :in as
      :nconc (loop
                :for b :in bs
                :collect (coerce (list a b) 'string)))))

#+(or)
(cross "abc" "123")
#+(or)
(cross (coerce "abc" 'list) (coerce "123" 'list))
#+(or)
(cross #\a "1234")
#+(or)
(cross "abcd" #\1)
#+(or)
(cross #\a #\1)

(defparameter *digits* "123456789")

(defparameter *rows* "ABCDEFGHI")

(defparameter *cols* *digits*)

(defparameter *squares* (cross *rows* *cols*))

(defparameter *unitlist* (append
                          (loop
                            :for c :across *cols*
                            :collect (cross *rows* c))
                          (loop
                            :for r :across *rows*
                            :collect (cross r *cols*))
                          (loop
                            :for rs :in (util:list->groups (coerce *rows* 'list) 3)
                            :nconc (loop
                                     :for cs :in (util:list->groups (coerce *cols* 'list) 3)
                                     :collect (cross rs cs)))))

(defparameter *units* (loop
                        :with h := (make-hash-table :test #'equal)
                        :for s :in *squares*
                        :do (setf (gethash s h)
                                  (remove-if-not (lambda (u) (member s u :test #'equal))
                                                 *unitlist*))
                        :finally (return h)))

(defparameter *peers* (loop
                        :with h := (make-hash-table :test #'equal)
                        :for s :in *squares*
                        :do (setf (gethash s h)
                                  (remove-duplicates (remove s
                                                             (apply #'append
                                                                    (gethash s *units*))
                                                             :test #'equal)
                                                     :test #'equal))
                        :finally (return h)))

;; Parse a grid

(defun parse-grid (grid)
  "Return NIL if contradiction is detected."
  (let* ((digits (coerce *digits* 'list))
         (vals (a:alist-hash-table (loop
                                     :for s in *squares*
                                     :collect (cons s digits))
                                   :test #'equal))
         (grid-vals (grid-values grid)))
    (loop
      :for s :being :the hash-key :of grid-vals
        :using (hash-value d)
      :when (and (digit-char-p d)
                 (not (assign vals s d)))
        :do (return))
    vals))

(defun grid-values (grid)
  "Convert `grid` into a square:char hashmap. Empty squares can be
represented by #\0 or #\."
  (flet ((ok? (char)
           (or (char<= #\0 char #\9)
               (char= #\. char))))
    (let ((chars (loop
                   :for c :across grid
                   :when (ok? c)
                     :collect c)))
      (assert (= 81 (length chars)) ()
              "Grid: ~a Chars: ~a Char count: ~a"
              grid chars (length chars))
      (a:alist-hash-table (mapcar #'cons *squares* chars)
                          :test #'equal))))

;; Constraint propagation

(defun assign (values s d)
  ""
  (when (every (a:curry #'eliminate values s)
               (remove d (gethash s values)))
    values))

(defun eliminate (values s d)
  ""
  ;; TODO: Fill this in
  nil)

;; Visualization

(defun display (values))

;; Test

(defparameter grid1 "003020600900305001001806400008102900700000008006708200002609500800203009005010300")

(defparameter grid2 "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......")

(defparameter hard1 ".....6....59.....82....8....45........3........6..3.54...325..6..................")
