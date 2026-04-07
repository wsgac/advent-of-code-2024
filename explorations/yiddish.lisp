(in-package #:explorations.yiddish)

(defvar *hunspell-yiddish-dictionary* "~/hack/lisp/yi.dic")

(defun read-dictionary (path)
  (let ((path (uiop:ensure-absolute-pathname path)))
    (coerce (serapeum:drop 2 (uiop:read-file-lines path)) 'vector)))

(defun has-letters? (word letters)
  (some (lambda (c) (find c word)) letters))

(defun solve-1 (dict)
  (let* ((5-letter (remove-if-not (lambda (s) (= 5 (length s))) dict))
         ;; (matching (remove-if-not (lambda (s) (str:ends-with? "רעס" s)) 5-letter))
         (matching (remove-if-not (lambda (s)
                                    (and (char= (elt s 2) #\hebrew_letter_bet)
                                         (char= (elt s 3) #\hebrew_letter_yod)
                                         (char= (elt s 0) #\hebrew_letter_nun)
                                         ;; (not (has-letters? s "קשדמגעט"))
                                         ))
                                  5-letter))
         )
    matching))
