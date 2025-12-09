(in-package #:advent-of-code-2024.day23)


(defun parse-input (input)
  (mapcar (lambda (row)
            (mapcar (a:compose #'a:make-keyword #'string-upcase)
                    (uiop:split-string row :separator '(#\-))))
          (util:split-lines input)))

(defun build-graph (connections)
  (loop
    with h = (make-hash-table)
    for (a b) in connections
    do (pushnew a (gethash b h))
    do (pushnew b (gethash a h))
    finally (return h)))

(defun problem-1 (&key (input *input-part-1-test*))
  (let* ((graph (build-graph (parse-input input)))
         (cliques (let (cliques)
                    (flet ((neighbors-fn (v)
                             (gethash v graph))
                           (visitor-fn (c)
                             (push (sort c #'string< :key #'symbol-name) cliques)))
                      (util:bron-kerbosch
                       nil (a:hash-table-keys graph) nil #'neighbors-fn #'visitor-fn)
                      cliques))))
    (loop
      with h = (make-hash-table :test #'equal)
      for clique in cliques
      when (<= 3 (length clique))
        do (loop
             for triplet in (util:choose-combinations-not-replacing clique 3)
             when (some (lambda (el) (str:starts-with? #\T (symbol-name el))) triplet)
               do (setf (gethash triplet h) t))
      finally (return (hash-table-count h)))))

(defun problem-2 (&key (input *input-part-1-test*))
  (let* ((graph (build-graph (parse-input input)))
         (cliques (let (cliques)
                    (flet ((neighbors-fn (v)
                             (gethash v graph))
                           (visitor-fn (c)
                             (push (sort c #'string< :key #'symbol-name) cliques)))
                      (util:bron-kerbosch
                       nil (a:hash-table-keys graph) nil #'neighbors-fn #'visitor-fn)
                      cliques))))
    (loop
      with max-clique = nil
      with max = 0
      for clique in cliques
      for clique-length = (length clique)
      when (> clique-length max)
        do (setf max clique-length
                 max-clique clique)
      finally (return (format nil "~{~(~a~)~^,~}" max-clique)))))

(defparameter *input-part-1-test*
  "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2024 23))
