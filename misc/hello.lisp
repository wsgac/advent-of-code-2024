(asdf:load-system :quicklisp)
(ql:quickload :local-time)

(defpackage #:hello-standalone
  (:use #:cl)
  (:local-nicknames
   (#:lt #:local-time)))

(in-package #:hello-standalone)

(defun hello ()
  (format t "Hello! Current time is: ~a" (lt:now)))

(sb-ext:save-lisp-and-die #p"/tmp/hello-standalone"
                          :toplevel #'hello
                          :executable t)
