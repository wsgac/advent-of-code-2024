(in-package :advent-of-code-2024.misc)

(define-condition alert ()
  ())

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
