(in-package #:advent-of-code-2022.day12)

;; Solution

(defun parse-input (input)
  (loop
    :with start
    :with end
    :with rows := (util:split-lines input)
    :with arr := (make-array (list (length rows)
                                   (length (first rows))))
    :for rownum :from 0
    :for row :in rows
    :do (loop
          :for colnum :from 0
          :for char :across row
          :do (setf (aref arr rownum colnum)
                    (case char
                      (#\S 0)
                      (#\E 25)
                      (otherwise (- (char-code char) (char-code #\a)))))
          :when (char= char #\S)
            :do (setf start (cons rownum colnum))
          :when (char= char #\E)
            :do (setf end (cons rownum colnum)))
    :finally (return (list :array arr
                           :start start
                           :end end))))

(tr:defun-ematch* euclid (p1 p2)
  (((cons x1 y1) (cons x2 y2)) (sqrt (+ (square (- x1 x2))
                                        (square (- y1 y2))))))

(defun neighbors (arr p)
  (loop
    :with (pr . pc) := p
    :with curr := (aref arr pr pc)
    :for (dr . dc) :in '((0 . 1) (1 . 0) (0 . -1) (-1 . 0))
    :when (ignore-errors (let ((neighbor (aref arr (+ pr dr) (+ pc dc))))
                           (<= (- neighbor curr) 1)))
      :collect (cons (+ pr dr) (+ pc dc))))

(defun cost (arr current next)
  (let ((d (- (aref arr (car next) (cdr next))
              (aref arr (car current) (cdr current)))))
   (if (<= d 1)
       1 most-positive-fixnum)))

(defun path (start end came-from)
  (loop
    :with path
    :for curr := end :then (gethash curr came-from)
    :until (equal curr start)
    :do (push curr path)
    :finally (return (nreverse path))))

(defun a* (arr start end)
  (let ((frontier (make-instance 'u:priority-queue-heap
                                    :cmp #'< :key #'second))
           (came-from (make-hash-table :test #'equal))
           (cost-so-far (make-hash-table :test #'equal)))
       (loop
         :initially (progn (pq-push frontier (list start 0))
                           (setf (gethash start came-from) nil)
                           (setf (gethash start cost-so-far) 0))
         :while (and (not (pq-empty? frontier))
                     (not (equal current end)))
         :for step :from 0 :below (array-total-size arr)
         :for current := (first (pq-pop frontier))
         ;; :do (format t "~%~a" current)
         :when (equal current end)
           :do (return)
         :do (loop
               :for next :in (neighbors arr current)
               :for new-cost := (+ (gethash current cost-so-far)
                                   (cost arr current next))
               :when (or (not (nth-value 1 (gethash next cost-so-far)))
                         (< new-cost (gethash next cost-so-far)))
                 :do (progn (setf (gethash next cost-so-far) new-cost)
                            (pq-push frontier (list next
                                                    (+ new-cost (euclid next end))))
                            (setf (gethash next came-from) current))))
       (path start end came-from)))

(defun problem-1 (&key (input *input-part-1-test*))
  (tr:match (parse-input input)
    ((tr:plist :array arr :start start :end end)
     (length (a* arr start end)))))

(defun find-starting-points (arr end dist)
  (let (s)
    (u:array-loop (arr (row col) :item el)
      (when (and (zerop el) (<= (euclid end (cons row col)) dist))
        (push (cons row col) s)))
    s))

(defun neighbors-inv (arr p)
  (loop
    :with (pr . pc) := p
    :with curr := (aref arr pr pc)
    :for (dr . dc) :in '((0 . 1) (1 . 0) (0 . -1) (-1 . 0))
    :when (ignore-errors (let ((neighbor (aref arr (+ pr dr) (+ pc dc))))
                           (>= (- neighbor curr) -1)))
      :collect (cons (+ pr dr) (+ pc dc))))

(defun bfs (arr start)
  (flet ((at-zero (p)
           (zerop (aref arr (car p) (cdr p)))))
   (let ((frontier (sb-concurrency:make-queue))
         (came-from (make-hash-table :test #'equal)))
     (loop
       :initially (progn (sb-concurrency:enqueue start frontier)
                         (setf (gethash start came-from) nil))
       :while (not (sb-concurrency:queue-empty-p frontier))
       :for current := (sb-concurrency:dequeue frontier)
       :do (loop
             :for next :in (neighbors-inv arr current)
             :when (not (nth-value 1 (gethash next came-from)))
               :do (sb-concurrency:enqueue next frontier)
               :and :do (setf (gethash next came-from) current))
       :when (at-zero current)
         :do (return-from bfs (path start current came-from))))))

(defun problem-2 (&key (input *input-part-1-test*))
  (tr:match (parse-input input)
    ((tr:plist :array arr :end end)
     ;; (let ((lparallel:*kernel* (lparallel:make-kernel 16)))
     ;;   (reduce #'min
     ;;           (common-lisp:mapcar (lambda (s) (let ((res (a* arr s end)))
     ;;                                      (length res)))
     ;;                        (find-starting-points arr end 20))))
     ;; (loop
     ;;   :with p
     ;;   :for s :in (find-starting-points arr end 15)
     ;;   :do (format t "~%Start: ~a " s)
     ;;   :do (setf p (a* arr s end))
     ;;   :do (format t "Result: ~a~%" (length p))
     ;;   :minimize (length p))
     (length (bfs arr end))
     )))

;; Data

(defparameter *input-part-1-test*
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")


(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  "abcccaaaaaaccccccccaaaaaccccccaaaaaaccccccaaaaaaaacccaaaaaaaccaaaacccccccccccccccccccccccccaaaaaacccccccccccccccccccccccccccccaaaaaa
abcccaaaaaacccccccaaaaaaccccaaaaaaaacccccccaaaaaaaaaaaaaaaaccaaaaacccccccccccccccccccccccccaaaaaacccccccccccccccccccccccccccccaaaaaa
abccccaaaaacaaaccaaaaaaaacccaaaaaaaaacccccccaaaaaaaaaaaaaaaacaaaaaacccccccccaaacccccccccccaaaaaaaaccccccccccaaccccccccccccccccaaaaaa
abccccaaaaccaaaaaaaaaaaaacccaaaaaaaaaacccccaaaaaaaaaaaaaaaaaaacaaaacccccccccaaaacccccccccaaaaaaaaaacccccccccaaaccccccccccccccccccaaa
abcccccccccaaaaaacccaacccccccccaaacaaaccccccaacccccccaaaaaaaaacaacccccccccccaaaacccccccccaaaaaaaaaacccccccccaaaccacaaccccccccccccaaa
abcccccccccaaaaaacccaacccccccccaaacccccccccccccccccccaaaacaaaacccccccaacaaccaaaccccccccccaccaaaaacacccccccccaaaacaaaaccccccccccccaac
abccccccccccaaaaacccccccccccccccacccaaaacccccccccccccaaaacccccccccccccaaaacccccccccccaacccccaaaaccccccccjjjjaaaaaaaaaccccccccccccccc
abccccccccccaaaacccccccccccccccccccaaaaacccccccccccccaaaccccccccccccccaaaaacccccccccaaaaaacccaaccccccccjjjjjjkkaaaacccccccccaacccccc
abcccccaaccccccccccccccccccccccccccaaaaaacccccccccccccaacccccccccccccaaaaaaccccccccccaaaaaccccccccccccjjjjjjjkkkkaacccccaacaaacccccc
abccaaaacccccccccccccccccccccccccccaaaaaaccccccccccccccccccccccccccccaaaacaccccccccaaaaaaaccccaacccccjjjjoooookkkkkkkklllaaaaaaacccc
abccaaaaaacccccccccccccccccccccccccaaaaacccccccccccccccccccccccccccccccaaccccccccccaaaaaaaaccaaaaccccjjjoooooookkkkkkkllllaaaaaacccc
abcccaaaaacccccccccccccccccccccccccccaaaccccccccaaaacccccccccccccccccccccccccccccccaaaaaaaaccaaaaccccjjooooooooppkkppplllllaccaacccc
abccaaaaaccccccccccccaccccccccccccccccccccccccccaaaacccccccccccccccccccccccccccccccccaaacacccaaaacccijjooouuuuoppppppppplllccccccccc
abcccccaacccccccccccaaaaaaaaccccccccccccccccccccaaaaccccaaccccccccaaacccccccccccccaacaaccccccccccccciijoouuuuuuppppppppplllcccaccccc
abcccccccccccccccccccaaaaaaccccccccccccccccccccccaaccccaaaacccccccaaaaccccccccccaaaaaaccccccccccccciiiiootuuuuuupuuuvvpppllccccccccc
abcccccccccccccccccccaaaaaaccaaaaacccccccccccccccccccccaaaacccccccaaaaccccccccccaaaaaaccccccccccccciiinnotuuxxxuuuuvvvpppllccccccccc
abccccccccccccccacccaaaaaaaacaaaaaaacccccccccccccccccccaaaacccccccaaacccccaaaaccaaaaaccccaaccccccciiiinnnttxxxxuuyyyvvqqqllccccccccc
abcccccccccccaaaaccaaaaaaaaaaaaaaaaaaccaacccccccccccccccccccccccccccccccccaaaacccaaaaaccaaacccccciiinnnnnttxxxxxyyyyvvqqqllccccccccc
abaaaacccccccaaaaaaaaaaaaaaaaaaaaaaaaaaaacccccccccccccccccccccccccccccccccaaaacccaaaaaacaaaccccciiinnnnttttxxxxxyyyyvvqqmmmccccccccc
abaaaaccccccccaaaaacccaaaaacaaaaaacaaaaaaccccccccccccccccaaccccccccccccccccaacccccccaaaaaaaaaaciiinnnnttttxxxxxyyyyvvqqqmmmccccccccc
SbaaaacccccccaaaaaccccaaaaaccaaaaaaaaaaaccccccccccccccccaaacaacccccccccccccccccccccccaaaaaaaaachhhnnntttxxxEzzzzyyvvvqqqmmmccccccccc
abaaaacccccccaacaacccccaaaaaaaacaaaaaaaaaccccccccccccccccaaaaaccccccccccccccccccccccccaaaaaaacchhhnnntttxxxxxyyyyyyvvvqqmmmdddcccccc
abaaaacccccccccccccccccccaaaaaacaaaaaaaaaacccccccccccccaaaaaaccccccccaaaccccccccccccccaaaaaaccchhhnnntttxxxxywyyyyyyvvvqqmmmdddccccc
abaacccccccccccccccccccaaaaaaacccccaaaaaaacccccccccccccaaaaaaaacccccaaaacccccccccccccaaaaaaacaahhhmmmttttxxwwyyyyyyyvvvqqmmmdddccccc
abcccccccccccccccccccccaaaaaaacaaccaaacccccccccccccccccaacaaaaacccccaaaacccccccccccccaaacaaaaaahhhmmmmtsssswwyywwwwvvvvqqqmmdddccccc
abcccccccccccccccaaaccccaaaaaaaaaacaaccaaccccccccccccccccaaacaccccccaaaacccccccccccccccccaaaaacahhhmmmmmsssswwywwwwwvvrrqqmmdddccccc
abcccccccccccccaaaaaaccccaaaaaaaaaccaaaacccccccccccccccccaacccccccccccccccccccccccaaaccccaaaaaaahhhhhmmmmssswwwwwrrrrrrrrmmmmddccccc
abcccccccccccccaaaaaaccccaaaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccaaaaaacccccaaaaachhhhhmmmmsswwwwrrrrrrrrrkkmdddccccc
abccccccccccccccaaaaaccccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccaaaaaaccccaaaaacccchhggmmmssswwrrrrrkkkkkkkkdddacccc
abccaaaacccccccaaaaacccccccccaaaaaacaaaaacccccccccccccccccccccccccccccccccccccccaaaaaaccccaacaaaccccggggmmsssssrrlkkkkkkkkkdddaccccc
abccaaaacccccccaaaaacccccccccaaaaaaccccaacccccccccccccccccccccccccccccccccccccccaaaaaccccccccaaccccccgggmllssssrllkkkkkkkeeeddaccccc
abccaaaacccccccaaacccccccccccaaaaaacccccccccccccccccccaacccccccccccccccccccccccaaaaaacccccccccccccccccggllllssslllkkeeeeeeeeeaaacccc
abcccaaccccccccaaacaaaccccccaaaaaaaaaaacccccccccccccaaaaaacccccccccccccccccccccaaacaaacccccaacccccccccggglllllllllfeeeeeeeeaaaaacccc
abccccccccccaaaaaaaaaaccccccccccccaccaaaccacccccccccaaaaaaccccaaccaacccaaccccccaaaaaaacccccaaccccccccccggglllllllfffeeecccaaaaaacccc
abccccccccccaaaaaaaaacccccccccccccccaaaaaaaccccccccccaaaaaccccaaaaaacccaaaaaaccaaaaaacccaaaaaaaacccccccggggllllfffffccccccaacccccccc
abcccccccccccaaaaaaacccccccccccccccccaaaaaaccaacccccaaaaaccccccaaaaacccaaaaaacaaaaaaacccaaaaaaaaccccccccgggffffffffccccccccccccccccc
abccccccccccccaaaaaaacccccccccccccaaaaaaaaacaaaaccccaaaaacaaaaaaaaaacaaaaaaacaaaaaaaaaccccaaaacccccccccccggffffffacccccccccccccccaaa
abccccccccccccaaaaaaacaaccccccccccaaaaaaaaacaaaacccccaaaaaaaaaaaaaaaaaaaaaaacaaaaaaaaaacccaaaaacccccccccccaffffaaaaccccccccccccccaaa
abccccccccccccaaacaaaaaacccccccccccaaaaaaaacaaaaaaaaaaaaaaaaaaaaaaaaacaaaaaaacccaaacaaaccaaaaaacccccccccccccccccaaaccccccccccccccaaa
abccccccccccccaaccaaaaaccccccccccccccaaaaaaaccccaaaaaaaaaaaaccccaacccccaaaaaacccaaaccccccaaccaacccccccccccccccccaaacccccccccccaaaaaa
abcccccccccccccccaaaaaaaaccccccccccccaacccacccccccaaaaaaaaaaccccaacccccaaccccccccaccccccccccccccccccccccccccccccccccccccccccccaaaaaa")
