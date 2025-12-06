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
  (u:get-problem-data 2022 5))
