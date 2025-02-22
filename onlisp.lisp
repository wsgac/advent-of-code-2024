(in-package :advent-of-code-2024.onlisp)

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
               #'(lambda (&rest args)
                   (apply #'rmapcar fn args))
               args)))

;; (defmacro with-gensyms (symbols &body body)
;;   `(let ,(loop for s in symbols
;;                collect (list s (gensym)))
;;      ,@body))

(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc #'(lambda ,parms ,@body)
                  ,@(map0-n #'(lambda (n)
                                `(nthcdr ,n ,src))
                            (1- (length parms))))))))

;; WSG: Simplified implementation
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
            (mapc #'(lambda ,parms ,@body)
                  ,@(map0-n #'(lambda (n)
                                `(append (nthcdr ,n ,src)
                                         (subseq ,src 0 ,n)))
                            (1- (length parms))))))))

;; Original On Lisp implementation
#+(or)
(defmacro do-tuples/c (parms source &body body)
  (if parms
      (alexandria:with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar #'(lambda (args)
                                    `(,bodfn ,@args))
                                  (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n #'(lambda (n)
                                         `(nth ,(1- n)
                                               ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
              (map1-n #'(lambda (n)
                          (let ((x (+ m n)))
                            (if (>= x len)
                                `(nth ,(- x len) ,src)
                                 `(nth ,(1- x) ,rest))))
                        len))
          (- len 2)))

;; Multiple-value DO
(defmacro mvdo* (var-clauses end-clauses &body body)
  "Just like `do`, but allows for multiple-value bindings inside
`var-clauses`."
  (mvdo-gen var-clauses var-clauses end-clauses body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
            ,label
            (when ,(first test)
              (return (progn ,@(rest test))))
            ,@body
            ,@(mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (mvdo-gen (rest binds) rebinds test body))
            (var/s (caar binds))
            (expr (cadar binds)))
        (if (atom var/s)
            `(let ((,var/s ,expr))
               ,rec)
            `(multiple-value-bind ,var/s ,expr
               ,rec)))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (first rebinds)) 3)
         (mvdo-rebind-gen (rest rebinds)))
        (t (cons (list (if (atom (caar rebinds))
                           'setq
                           'multiple-value-setq)
                       (caar rebinds)
                       (third (first rebinds)))
                 (mvdo-rebind-gen (rest rebinds))))))

(defun shuffle (l1 l2)
  "Intersperse elements from `l1` and `l2` until any of them is exhausted
and, appending any remaining elements to the end."
  (cond ((null l1) l2)
        ((null l2) l1)
        (t (list* (first l1)
                  (first l2)
                  (shuffle (rest l1)
                           (rest l2))))))

(defun group (list n)
  "Group `list` into groups of (at most) `n` elements. The last group is
permitted to contain a smaller number of elements. When `n` is zero,
return NIL."
  (when (plusp n)
    (loop
      for l on list by (lambda (x) (nthcdr n x))
      collect (subseq l 0 (min n (length l))))))

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar #'(lambda (p)
                           (mapcar #'(lambda (x)
                                       (declare (ignorable x))
                                       (gensym))
                                   (a:ensure-list (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq
                     ,@(mapcan #'(lambda (p s)
                                   (shuffle (a:ensure-list (car p))
                                            s))
                               pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
                                ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                               (mapcar #'(lambda (x)
                                           (declare (ignorable x))
                                           (gensym))
                                         (car b))
                               (gensym)))
                         binds)))
    `(let ,(mappend #'a:ensure-list temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                              (list var (cadr b)))
                            binds
                            temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
               (mappend #'a:ensure-list (mapcar #'car binds))
               (mappend #'a:ensure-list temps))
          ,label
          (if ,test
              (return (progn ,@result)))
          ,@body
          (mvpsetq ,@(mapcan #'(lambda (b)
                                 (if (third b)
                                     (list (car b)
                                           (third b))))
                               binds))
          (go ,label)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized Variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro allf (val &rest args)
  (a:with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)

;; Simple operations on list ends

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

;; More advanced macros

(defmacro _f (op place &rest args)
  "Call function or macro `op` on generalized variable `place`, setting
it to the obtained value. Pass any `args` to the call as well."
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let (,@(mapcar #'list vars forms)
           (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (a:with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))



;;;;;;;;;;;
;; Misc  ;;
;;;;;;;;;;;

(defun create-teams ()
  (flet ((group (list n)
           (when (plusp n)
             (loop
               for l on list by (lambda (x) (nthcdr n x))
               collect (subseq l 0 (min n (length l)))))))
   (let* ((people '(@bkc @bkn @gmo @awo @wzy @kzh @fje @maz @dkr @aeg))
          (shuffled (a:shuffle (copy-list people)))
          (topics '(:add-filter-idempotence :purge-stdio-logging :parsing-stack-status)))
     (values (mapcar #'list
                     (a:shuffle (copy-list topics))
                     (group shuffled 3))
             (car (last shuffled))))))
