(in-package #:advent-of-code-2022.day05)

(defun parse-input (input)
  (destructuring-bind (state-raw instr-raw)
      (str:split "

" input)
    (list (parse-state state-raw)
          (parse-instr instr-raw)
          )))

(defun parse-state (state-raw)
  (let* ((rows (util:split-lines state-raw))
         (stack-n (a:lastcar (util:parse-integers (a:lastcar rows)))))
    (loop
      :with stacks := (make-list stack-n)
      :for row :in (nreverse (butlast rows))
      :do (loop
            :for col :below stack-n
            :for el := (elt row (1+ (* col 4)))
            :unless (s:whitespacep el)
            :do (push el (nth col stacks)))
      :finally (return stacks))))

(defun parse-instr (instr-raw)
  (let (instr)
    (re:do-register-groups (n from to)
        ("move (\\d+) from (\\d+) to (\\d+)" instr-raw)
      (push (list :n (parse-integer n)
                  :from (1- (parse-integer from))
                  :to (1- (parse-integer to)))
            instr))
    (nreverse instr)))

(defun run-instr (state instr)
  (trivia:match instr
    ((list :n n :from from :to to)
     (loop
       :repeat n
       :do (push (pop (nth from state)) (nth to state))
       :finally (return state)))))

(defun problem-1 (&key (input *input-part-1-test*))
  (destructuring-bind (state instr) (parse-input input)
    (coerce
     (mapcar #'car (reduce #'run-instr instr :initial-value state))
     'string)))

(defun run-instr-2 (state instr)
  (trivia:match instr
    ((list :n n :from from :to to)
     (loop
       :with buffer
       :repeat n
       :do (push (pop (nth from state)) buffer)
       :finally (return (loop
                          :repeat n
                          :do (push (pop buffer) (nth to state))
                          :finally (return state)))))))

(defun problem-2 (&key (input *input-part-2-test*))
  (destructuring-bind (state instr) (parse-input input)
    (coerce
     (mapcar #'car (reduce #'run-instr-2 instr :initial-value state))
     'string)))

(defparameter *input-part-1-test*
  "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  "[V]     [B]                     [C]
[C]     [N] [G]         [W]     [P]
[W]     [C] [Q] [S]     [C]     [M]
[L]     [W] [B] [Z]     [F] [S] [V]
[R]     [G] [H] [F] [P] [V] [M] [T]
[M] [L] [R] [D] [L] [N] [P] [D] [W]
[F] [Q] [S] [C] [G] [G] [Z] [P] [N]
[Q] [D] [P] [L] [V] [D] [D] [C] [Z]
 1   2   3   4   5   6   7   8   9 

move 1 from 9 to 2
move 4 from 6 to 1
move 4 from 2 to 6
move 5 from 8 to 7
move 4 from 9 to 2
move 1 from 5 to 8
move 1 from 3 to 1
move 2 from 3 to 1
move 1 from 4 to 2
move 11 from 7 to 2
move 5 from 5 to 1
move 1 from 6 to 8
move 1 from 7 to 6
move 3 from 6 to 7
move 1 from 3 to 2
move 1 from 6 to 8
move 11 from 2 to 1
move 1 from 9 to 8
move 1 from 3 to 7
move 4 from 7 to 9
move 3 from 3 to 7
move 4 from 8 to 2
move 3 from 7 to 6
move 2 from 6 to 3
move 5 from 4 to 1
move 1 from 6 to 5
move 26 from 1 to 7
move 1 from 4 to 6
move 22 from 7 to 5
move 4 from 9 to 1
move 3 from 7 to 3
move 1 from 6 to 3
move 6 from 1 to 7
move 2 from 7 to 5
move 8 from 1 to 9
move 4 from 3 to 4
move 10 from 2 to 7
move 6 from 7 to 4
move 2 from 9 to 5
move 1 from 5 to 1
move 8 from 4 to 1
move 2 from 5 to 9
move 1 from 3 to 6
move 1 from 9 to 1
move 1 from 3 to 6
move 2 from 5 to 2
move 1 from 4 to 2
move 1 from 2 to 3
move 7 from 1 to 4
move 9 from 7 to 4
move 1 from 3 to 4
move 2 from 2 to 4
move 5 from 9 to 6
move 1 from 4 to 5
move 2 from 9 to 3
move 1 from 1 to 6
move 2 from 6 to 1
move 2 from 6 to 5
move 2 from 9 to 7
move 1 from 3 to 9
move 1 from 9 to 5
move 2 from 7 to 3
move 1 from 1 to 7
move 7 from 4 to 5
move 2 from 1 to 2
move 3 from 3 to 8
move 3 from 8 to 9
move 31 from 5 to 8
move 1 from 7 to 1
move 1 from 2 to 1
move 1 from 1 to 5
move 1 from 5 to 6
move 2 from 5 to 7
move 10 from 4 to 9
move 5 from 6 to 2
move 3 from 2 to 6
move 2 from 7 to 8
move 1 from 6 to 3
move 1 from 4 to 1
move 1 from 3 to 6
move 1 from 4 to 2
move 2 from 1 to 2
move 1 from 8 to 7
move 10 from 8 to 2
move 13 from 2 to 9
move 1 from 1 to 5
move 18 from 8 to 2
move 21 from 9 to 6
move 1 from 7 to 8
move 2 from 9 to 7
move 1 from 2 to 3
move 1 from 7 to 8
move 9 from 2 to 4
move 1 from 7 to 8
move 3 from 9 to 1
move 1 from 8 to 1
move 6 from 2 to 3
move 5 from 4 to 7
move 1 from 5 to 8
move 2 from 4 to 3
move 5 from 7 to 3
move 2 from 2 to 7
move 15 from 6 to 1
move 12 from 1 to 2
move 6 from 2 to 9
move 4 from 9 to 5
move 4 from 5 to 6
move 14 from 3 to 9
move 1 from 6 to 7
move 1 from 7 to 2
move 1 from 7 to 8
move 9 from 2 to 6
move 1 from 1 to 6
move 2 from 9 to 8
move 4 from 9 to 7
move 1 from 1 to 5
move 8 from 8 to 3
move 1 from 5 to 4
move 2 from 1 to 2
move 3 from 1 to 4
move 9 from 6 to 2
move 1 from 7 to 4
move 1 from 8 to 2
move 1 from 6 to 4
move 4 from 7 to 8
move 12 from 6 to 8
move 3 from 2 to 1
move 6 from 8 to 7
move 5 from 3 to 6
move 3 from 3 to 6
move 3 from 1 to 3
move 8 from 2 to 9
move 2 from 4 to 5
move 2 from 7 to 2
move 10 from 8 to 5
move 3 from 3 to 2
move 10 from 5 to 3
move 1 from 4 to 3
move 1 from 2 to 1
move 1 from 1 to 7
move 14 from 9 to 6
move 5 from 2 to 4
move 15 from 6 to 5
move 3 from 9 to 3
move 1 from 8 to 6
move 1 from 3 to 8
move 7 from 3 to 8
move 16 from 5 to 1
move 2 from 7 to 1
move 1 from 5 to 9
move 2 from 9 to 3
move 15 from 1 to 5
move 3 from 8 to 2
move 3 from 3 to 1
move 3 from 7 to 3
move 8 from 4 to 6
move 5 from 1 to 6
move 9 from 5 to 7
move 2 from 8 to 3
move 2 from 2 to 7
move 1 from 1 to 4
move 2 from 5 to 8
move 4 from 3 to 1
move 4 from 8 to 1
move 1 from 8 to 6
move 9 from 7 to 6
move 2 from 7 to 5
move 3 from 1 to 8
move 1 from 4 to 8
move 1 from 2 to 4
move 12 from 6 to 2
move 3 from 8 to 6
move 1 from 4 to 7
move 2 from 6 to 8
move 5 from 5 to 9
move 13 from 2 to 9
move 2 from 4 to 7
move 13 from 9 to 5
move 2 from 6 to 5
move 1 from 3 to 9
move 6 from 9 to 4
move 5 from 1 to 3
move 1 from 7 to 9
move 15 from 5 to 8
move 2 from 4 to 7
move 2 from 4 to 6
move 1 from 4 to 6
move 1 from 5 to 7
move 18 from 6 to 2
move 2 from 7 to 3
move 3 from 6 to 7
move 3 from 2 to 8
move 5 from 7 to 3
move 1 from 9 to 6
move 2 from 3 to 8
move 11 from 3 to 2
move 2 from 2 to 9
move 1 from 6 to 2
move 1 from 7 to 5
move 1 from 5 to 9
move 9 from 8 to 4
move 1 from 4 to 6
move 2 from 3 to 1
move 2 from 1 to 5
move 12 from 8 to 3
move 1 from 8 to 2
move 14 from 3 to 4
move 1 from 6 to 4
move 1 from 5 to 4
move 20 from 2 to 7
move 2 from 9 to 5
move 1 from 5 to 3
move 1 from 9 to 2
move 1 from 2 to 8
move 2 from 2 to 3
move 5 from 4 to 5
move 6 from 5 to 7
move 2 from 8 to 2
move 3 from 3 to 9
move 5 from 4 to 5
move 2 from 9 to 7
move 2 from 2 to 3
move 1 from 9 to 3
move 22 from 7 to 3
move 4 from 7 to 4
move 24 from 3 to 6
move 4 from 2 to 6
move 18 from 6 to 9
move 15 from 4 to 6
move 8 from 6 to 3
move 6 from 6 to 1
move 7 from 9 to 6
move 2 from 7 to 4
move 8 from 3 to 9
move 14 from 6 to 3
move 2 from 3 to 9
move 1 from 9 to 6
move 13 from 9 to 1
move 3 from 4 to 5
move 1 from 9 to 6
move 5 from 1 to 8
move 3 from 3 to 9
move 2 from 1 to 5
move 8 from 5 to 8
move 10 from 3 to 5
move 3 from 4 to 6
move 6 from 1 to 9
move 4 from 5 to 3
move 5 from 8 to 2
move 6 from 6 to 3
move 7 from 3 to 6
move 1 from 3 to 4
move 5 from 8 to 7
move 5 from 2 to 6
move 2 from 7 to 3
move 3 from 7 to 3
move 1 from 4 to 9
move 9 from 6 to 9
move 2 from 6 to 2
move 1 from 8 to 2
move 2 from 8 to 7
move 5 from 1 to 5
move 1 from 1 to 4
move 13 from 5 to 7
move 5 from 3 to 7
move 1 from 5 to 6
move 1 from 4 to 6
move 3 from 2 to 8
move 1 from 3 to 5
move 1 from 3 to 8
move 14 from 7 to 4
move 1 from 5 to 6
move 7 from 6 to 9
move 6 from 7 to 9
move 2 from 8 to 9
move 2 from 8 to 1
move 31 from 9 to 1
move 13 from 4 to 2
move 1 from 4 to 3
move 10 from 2 to 7
move 1 from 3 to 4
move 1 from 2 to 7
move 3 from 7 to 8
move 1 from 4 to 1
move 3 from 8 to 5
move 32 from 1 to 5
move 3 from 9 to 7
move 4 from 9 to 6
move 2 from 2 to 7
move 2 from 1 to 7
move 1 from 6 to 1
move 1 from 9 to 4
move 3 from 6 to 4
move 1 from 1 to 8
move 15 from 5 to 1
move 1 from 8 to 4
move 9 from 5 to 7
move 1 from 9 to 8
move 1 from 8 to 1
move 10 from 1 to 9
move 1 from 4 to 2
move 2 from 9 to 5
move 4 from 9 to 6
move 1 from 2 to 7
move 3 from 4 to 2
move 1 from 1 to 5
move 5 from 1 to 5
move 1 from 4 to 9
move 3 from 6 to 7
move 23 from 7 to 6
move 1 from 2 to 4
move 1 from 2 to 5
move 9 from 5 to 4
move 1 from 2 to 5
move 9 from 5 to 6
move 1 from 9 to 7
move 1 from 9 to 3
move 3 from 9 to 4
move 14 from 6 to 3
move 5 from 7 to 4
move 1 from 7 to 5
move 1 from 5 to 9
move 2 from 5 to 6
move 16 from 6 to 2
move 2 from 6 to 1
move 7 from 4 to 8
move 2 from 1 to 2
move 4 from 3 to 5
move 5 from 4 to 7
move 2 from 6 to 7
move 4 from 4 to 1
move 4 from 8 to 9
move 1 from 4 to 5
move 1 from 6 to 8
move 1 from 4 to 9
move 4 from 1 to 7
move 1 from 9 to 4
move 2 from 2 to 7
move 7 from 3 to 9
move 15 from 2 to 3
move 4 from 8 to 6
move 1 from 4 to 7
move 2 from 9 to 7
move 1 from 6 to 8
move 2 from 7 to 2
move 5 from 7 to 2
move 1 from 5 to 2
move 6 from 2 to 9
move 3 from 7 to 1
move 3 from 1 to 2
move 3 from 7 to 1
move 2 from 2 to 9
move 2 from 6 to 9
move 1 from 8 to 3
move 19 from 3 to 9
move 1 from 6 to 3
move 3 from 7 to 4
move 1 from 2 to 5
move 2 from 1 to 9
move 2 from 2 to 3
move 33 from 9 to 7
move 1 from 1 to 7
move 3 from 3 to 7
move 1 from 3 to 2
move 1 from 5 to 8
move 4 from 9 to 7
move 1 from 5 to 2
move 2 from 4 to 9
move 4 from 9 to 7
move 3 from 2 to 1
move 1 from 4 to 3
move 1 from 9 to 7
move 1 from 8 to 3
move 7 from 7 to 3
move 3 from 1 to 9
move 4 from 9 to 7
move 4 from 5 to 8
move 3 from 3 to 4
move 3 from 4 to 5
move 3 from 3 to 6
move 2 from 6 to 5
move 38 from 7 to 5
move 40 from 5 to 3
move 4 from 8 to 9
move 1 from 6 to 9
move 1 from 5 to 1
move 3 from 7 to 6
move 1 from 7 to 5
move 38 from 3 to 8
move 1 from 1 to 9
move 3 from 9 to 6
move 5 from 3 to 9
move 4 from 8 to 6
move 1 from 7 to 1
move 3 from 5 to 9
move 1 from 1 to 2
move 10 from 8 to 3
move 5 from 8 to 1
move 3 from 1 to 2
move 9 from 6 to 7
move 9 from 3 to 5
move 1 from 7 to 6
move 1 from 3 to 8
move 1 from 7 to 9
move 1 from 1 to 5
move 1 from 1 to 3
move 1 from 9 to 2
move 4 from 2 to 3
move 1 from 2 to 4
move 9 from 8 to 1
move 2 from 9 to 5
move 2 from 1 to 2
move 2 from 3 to 4
move 6 from 8 to 6
move 10 from 5 to 3
move 7 from 3 to 2
move 2 from 1 to 2
move 5 from 1 to 7
move 7 from 9 to 6
move 7 from 6 to 5
move 1 from 4 to 3
move 7 from 7 to 4
move 5 from 3 to 9
move 7 from 2 to 6
move 4 from 7 to 8
move 5 from 8 to 9
move 1 from 2 to 6
move 1 from 3 to 5
move 2 from 2 to 8
move 8 from 4 to 6
move 7 from 9 to 7
move 4 from 7 to 9
move 7 from 9 to 3
move 8 from 3 to 1
move 6 from 5 to 9
move 8 from 1 to 8
move 13 from 8 to 4
move 3 from 9 to 6
move 1 from 8 to 6
move 1 from 7 to 3
move 2 from 4 to 1
move 5 from 9 to 1
move 1 from 3 to 7
move 15 from 6 to 1
move 1 from 7 to 9
move 10 from 4 to 7
move 11 from 7 to 5
move 17 from 1 to 6
move 1 from 9 to 3
move 6 from 6 to 1
move 3 from 5 to 3
move 2 from 4 to 5
move 2 from 7 to 8
move 12 from 5 to 3
move 13 from 6 to 9
move 2 from 8 to 2
move 2 from 5 to 1
move 16 from 3 to 8
move 3 from 2 to 3
move 2 from 3 to 7
move 2 from 7 to 9
move 1 from 3 to 7
move 4 from 8 to 4
move 2 from 4 to 8
move 5 from 1 to 5
move 2 from 4 to 7
move 6 from 6 to 8
move 2 from 8 to 5
move 2 from 1 to 4
move 5 from 8 to 7
move 5 from 6 to 3
move 6 from 9 to 8
move 2 from 9 to 2
move 1 from 1 to 7
move 4 from 5 to 3
move 2 from 2 to 3
move 1 from 4 to 9
move 10 from 3 to 6
move 1 from 3 to 7
move 10 from 7 to 2
move 2 from 5 to 3
move 1 from 4 to 2
move 2 from 6 to 8
move 3 from 6 to 5
move 1 from 6 to 1
move 7 from 2 to 3
move 6 from 8 to 7
move 4 from 6 to 3
move 14 from 8 to 6
move 11 from 6 to 8
move 1 from 1 to 4
move 6 from 7 to 2
move 3 from 5 to 8
move 4 from 1 to 7
move 1 from 2 to 8
move 1 from 2 to 6
move 1 from 3 to 4
move 1 from 5 to 6
move 7 from 8 to 6
move 9 from 3 to 2
move 1 from 8 to 5")
