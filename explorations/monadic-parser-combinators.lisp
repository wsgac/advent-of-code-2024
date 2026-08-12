(in-package #:monadic-parser-combinators)

;;;; This is code adapted from the paper "Monadic Parser Combinators"
;;;; by Meijer and Hutton, as transcribed here:
;;;; https://github.com/drewc/smug/blob/master/doc/monparsing.org

;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Parsers ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; result - always return parameter

;; result :: a -> Parser a
;; result v = \inp -> [(v,inp)]

(defun result (value)
  (lambda (input)
    (declare (type string input))
    (list (cons value input))))

;; zero - always fail

;; zero :: Parser a
;; zero = \inp -> []

(defun zero ()
  (constantly nil))

;; item - return first item whenever applicable

;; item :: Parser Char
;; item = \inp -> case inp of
;;                 [] -> []
;;                 (x:xs) -> [(x,xs)]

(defun item ()
  (lambda (input)
    (declare (type string input))
    (etypecase input
      (null nil)
      (string
       (unless (alexandria:emptyp input)
         (list (cons (elt input 0)
                     (subseq input 1))))))))

;; Test

(5am:test test-primitive-parsers
  (5am:is (equal (funcall (result 1) "asdf")
                 '((1 . "asdf"))))
  (5am:is (null (funcall (zero) "asdf")))
  (5am:is (equal (funcall (item) "asdf")
                 '((#\a . "sdf"))))
  (5am:is (equal (funcall (item) "a")
                 '((#\a . ""))))
  (5am:is (null (funcall (item) ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsers Combinators ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sequencing

;; seq     :: Parser a -> Parser b -> Parser (a,b)
;; p ‘seq‘ q = \inp -> [((v,w),inp’’) | (v,inp’) <- p inp
;;                                    , (w,inp’’) <- q inp’]

(defun seq (p1 p2)
  (lambda (input)
    (loop
      :for (result-1 . input-1)
        :in (funcall p1 input)
      :append (loop
                :for (result-2 . input-2)
                  :in (funcall p2 input-1)
                :collect (cons (cons result-1 result-2)
                               input-2)))))

;; Test

(5am:test test-parser-sequencing
  (5am:is (equal (funcall (seq (item) (item)) "asdf")
                 '(((#\a . #\s) . "df"))))
  (5am:is (equal (funcall (reduce #'seq (list (item) (item) (item) (item))) "asdf")
                 '(((((#\a . #\s) . #\d) . #\f) . "")))))

;; Bind - monadic sequencing

;; bind :: Parser a -> (a -> Parser b) -> Parser b
;; p ‘bind‘ f = \inp -> concat [f v inp’ | (v,inp’) <- p inp]

(defun bind (parser function)
  (lambda (input)
    (loop
      :for (value . input)
        :in (funcall parser input)
      :append (funcall (funcall function value) input))))

(5am:test test-parser-bind
  (5am:is (equal (funcall (bind (item) (lambda (c)
                                         (result (char-upcase c))))
                          "asdf")
                 '((#\A . "sdf"))))
  (5am:is (equal (funcall (bind (bind (item) (lambda (c)
                                               (result (char-upcase c))))
                            (lambda (c)
                              (result (char-downcase c))))
                          "asdf")
                 '((#\a . "sdf")))))

;; Implement `seq' using `bind'

(defun seq-bind (p1 p2)
  (bind p1
    (lambda (x)
      (bind p2
        (lambda (y)
          (result (cons x y)))))))

(5am:test test-alternate-parser-sequencing
  (5am:is (equal (funcall (seq-bind (item) (item)) "asdf")
                 '(((#\a . #\s) . "df"))))
  (5am:is (equal (funcall (reduce #'seq-bind (list (item) (item) (item) (item))) "asdf")
                 '(((((#\a . #\s) . #\d) . #\f) . "")))))

;; Sat

;; sat :: (Char -> Bool) -> Parser Char
;; sat p = item ‘bind‘ \x ->
;; if p x then result x else zero

(defun sat (predicate)
  (bind (item)
    (lambda (c)
      (if (funcall predicate c)
          (result c)
          (zero)))))

(5am:test test-sat
  (5am:is (null (funcall (sat #'digit-char-p) "asdf")))
  (5am:is (equal (funcall (sat #'digit-char-p) "1asdf")
                 '((#\1 . "asdf")))))

;; Specific character parsers

;; char :: Char -> Parser Char
;; char x = sat (\y -> x == y)

(defun char-parser (char)
  (sat (a:curry #'char= char)))

;; digit :: Parser Char
;; digit = sat (\x -> ’0’ <= x && x <= ’9’)

(defun digit-parser ()
  (sat #'digit-char-p))

;; lower :: Parser Char
;; lower = sat (\x -> ’a’ <= x && x <= ’z’)

(defun lower-parser ()
  (sat #'lower-case-p))

;; upper :: Parser Char
;; upper = sat (\x -> ’A’ <= x && x <= ’Z’)

(defun upper-parser ()
  (sat #'upper-case-p))

(5am:test test-specific-character-parsers
  ;; raw
  (5am:is (equal (funcall (char-parser #\a) "asdf")
                 '((#\a . "sdf"))))
  (5am:is (null (funcall (char-parser #\a) "xasdf")))
  (5am:is (null (funcall (digit-parser) "asdf")))
  (5am:is (equal (funcall (digit-parser) "1asdf")
                 '((#\1 . "asdf"))))
  (5am:is (equal (funcall (upper-parser) "ASDF")
                 '((#\A . "SDF"))))
  (5am:is (null (funcall (upper-parser) "asdf")))
  (5am:is (equal (funcall (lower-parser) "asdf")
                 '((#\a . "sdf"))))
  (5am:is (null (funcall (lower-parser) "ASDF")))
  ;; combined
  (let ((two-lower (bind (lower-parser)
                      (lambda (x)
                        (bind (lower-parser)
                          (lambda (y)
                            (result (coerce (list x y) 'string))))))))
    (5am:is (equal (funcall two-lower "asdf") '(("as" . "df"))))
    (5am:is (null (funcall two-lower "ASDF")))))

;; plus

;; plus :: Parser a -> Parser a -> Parser a
;; p ‘plus‘ q = \inp -> (p inp ++ q inp)

(defun plus (p1 p2)
  (lambda (input)
    (append (funcall p1 input)
            (funcall p2 input))))

(defun letter ()
  (plus (upper-parser) (lower-parser)))

(defun alphanum ()
  (plus (letter) (digit-parser)))

(5am:test test-plus-parser
  (let ((alnum (reduce #'plus (list (upper-parser) (lower-parser) (digit-parser)))))
    (5am:is (equal (funcall alnum "asdf")
                   '((#\a . "sdf"))))
    (5am:is (equal (funcall alnum "ASDF")
                   '((#\A . "SDF"))))
    (5am:is (equal (funcall alnum "1asdf")
                   '((#\1 . "asdf"))))))

;; Word parsing

;; word :: Parser String
;; word = neWord ‘plus‘ result ""
;; where
;; neWord = letter ‘bind‘ \x ->
;;   word ‘bind‘ \xs ->
;;   result (x:xs)

(defun word ()
  (flet ((ne-word ()
           (bind
               (letter)
             (lambda (x)
               (bind (word)
                 (lambda (xs)
                   (result (cons x xs))))))))
    (plus (ne-word) (result nil))))

(5am:test test-word-parsing
  (loop
    :with target := "eeny meeny"
    :for i :from 4 :downto 0
    :for (match . remainder) :in (funcall (word) target)
    :do (5am:is (string= (coerce match 'string)
                         (subseq target 0 i)))))

