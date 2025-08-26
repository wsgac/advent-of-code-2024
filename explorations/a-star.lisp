(in-package #:advent-of-code-2024.misc)

;;; This is a set of explorations about A* and related algorithms,
;;; based on
;;; https://www.redblobgames.com/pathfinding/a-star/introduction.html

(defparameter *grid-1*
  (make-array '(15 30)
              :initial-contents '((nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil)
                                  (nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil)
                                  (nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t   t   t   t nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t   t   t   t nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil   t   t nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  (nil nil nil nil nil nil nil nil nil nil nil nil   t   t nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
                                  )))

(defun neighbors (array current)
  (destructuring-bind (row col) current
    (loop
      :with (rows cols) := (array-dimensions array)
      :for (dr dc) :in '((-1 0) (1 0) (0 -1) (0 1))
      :for (nrow ncol) := `(,(+ row dr) ,(+ col dc))
      :when (and (< -1 nrow rows)
                 (< -1 ncol cols)
                 (not (aref array nrow ncol)))
        :collect `(,nrow ,ncol))))

(defun flood-fill (array start &key (on-visit
                                     (lambda (row col) (format t "~a~%" (cons row col)))))
  (let ((frontier (sb-concurrency:make-queue :name "frontier"
                                             :initial-contents (list start)))
        (reached (a:alist-hash-table `((,start . t)) :test #'equal)))
    (loop
      :while (not (sb-concurrency:queue-empty-p frontier))
      :for current := (sb-concurrency:dequeue frontier)
      :do (apply on-visit current)
      :do (loop
            :for next :in (neighbors array current)
            :unless (gethash next reached)
              :do (sb-concurrency:enqueue next frontier)
            :do (setf (gethash next reached) t)))))

(defun extract-path (came-from end)
  (loop
    :for point := end :then (gethash point came-from)
    :while point
    :collect point :into points
    :finally (return (nreverse points))))

(defun bfs-shortest-path (array start end)
  (let ((frontier (sb-concurrency:make-queue :name "frontier"
                                             :initial-contents (list start)))
        (came-from (a:alist-hash-table `((,start . nil)) :test #'equal)))
    (loop
      ;; :for i :from 1
      :while (not (sb-concurrency:queue-empty-p frontier))
      :for current := (sb-concurrency:dequeue frontier)
      :do (loop
            :for next :in (neighbors array current)
            :unless (nth-value 1 (gethash next came-from))
              :do (progn (sb-concurrency:enqueue next frontier)
                         (setf (gethash next came-from) current)))
      ;; :finally (format t "Steps: ~a~%" i)
      )
    (extract-path came-from end)))

(defun bfs-shortest-path-early-exit (array start end)
  (let ((frontier (sb-concurrency:make-queue :name "frontier"
                                             :initial-contents (list start)))
        (came-from (a:alist-hash-table `((,start . nil)) :test #'equal)))
    (loop
      ;; :for i :from 1
      :while (not (sb-concurrency:queue-empty-p frontier))
      :for current := (sb-concurrency:dequeue frontier)
      :when (equal current end)
        :do (progn
              ;; (format t "Steps: ~a~%" i)
              (return))
      :do (loop
            :for next :in (neighbors array current)
            :unless (nth-value 1 (gethash next came-from))
              :do (progn (sb-concurrency:enqueue next frontier)
                         (setf (gethash next came-from) current)))
      ;; :finally (format t "Steps: ~a~%" i)
      )
    (extract-path came-from end)))
