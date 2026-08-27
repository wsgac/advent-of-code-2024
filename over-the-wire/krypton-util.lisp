(in-package #:krypton)

(defun histogram (string)
  "Run frequency analysis on `string'. Return it as an alist."
  (loop
    :with h := (make-hash-table)
    :for c :across string
    :when (alpha-char-p c)
      :do (incf (gethash (char-downcase c) h 0))
    :finally (return (sort (a:hash-table-alist h) #'> :key #'cdr))))

(defun histogram (string)
  "Run frequency analysis on `string'. Return it as an alist."
  (loop
    :with h := (make-hash-table)
    :for c :across string
    :when (uiop/cl:alpha-char-p c)
      :do (incf (gethash (char-downcase c) h 0))
    :finally (return (sort (alexandria:hash-table-alist h) #'> :key #'cdr))))

(defun rotate (string shift)
  "Rotate, in the sense of Caesar's cipher, `string' by `shift'
characters."
  (flet ((rotate-char (c)
           (cond ((upper-case-p c)
                  (let ((a (char-code #\A)))
                    (code-char (+ (mod (+ (- (char-code c) a) shift) 26) a))))
                 ((lower-case-p c)
                  (let ((a (char-code #\a)))
                    (code-char (+ (mod (+ (- (char-code c) a) shift) 26) a))))
                 (t c))))
    (map 'string #'rotate-char string)))
