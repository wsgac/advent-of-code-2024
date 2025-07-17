(in-package :advent-of-code-2024.misc)

#+(or)
(define-condition alert ()
  ())

#+(or)
(handler-case
    (handler-case
        (handler-case
            (handler-case
                (error 'alert)
              (alert (e) (print "level 4")
                (error e)))
          (alert (e) (print "level 3")
            (error e)))
      (alert (e) (print "level 2")
        (error e)))
  (alert () (print "level 1")))


;;;;;;;;;;;;
;; Images ;;
;;;;;;;;;;;;

(defun generate-random-image (&key (width 150) (height 100) file)
  (let* ((png (make-instance 'zpng:png
                             :color-type :grayscale
                             :height height
                             :width width))
         (data (zpng:data-array png)))
    (dotimes (r height)
      (dotimes (c width)
        (setf (aref data r c 0)
              (random 255))))
    (if file
        (zpng:write-png png file)
        (uiop:with-temporary-file (:pathname p :type :png :keep t)
          (zpng:write-png png p)
          p))))
