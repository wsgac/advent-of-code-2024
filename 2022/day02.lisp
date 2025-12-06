(in-package #:advent-of-code-2022.day02)

(defun parse-input (input)
  (loop
    :with split := (uiop:split-string input :separator '(#\space #\newline))
    :for (opponent you) :on split :by #'cddr
    :collect (mapcar #'letter-to-item `(,opponent ,you))))

(defun letter-to-item (letter)
  (case (a:make-keyword letter)
    (:a :rock)
    (:b :paper)
    (:c :scissors)
    (:x :rock)
    (:y :paper)
    (:z :scissors)))

(defun item-to-score (item)
  (case item
    (:rock 1)
    (:paper 2)
    (:scissors 3)))

(defun battle-score (opponent you)
  (+ (cond
       ;; win
       ((member (list opponent you) '((:rock :paper) (:paper :scissors) (:scissors :rock))
                :test #'equal) 6)
       ;; draw
       ((eql opponent you) 3)
       ;; lose
       (t 0))
     (item-to-score you)))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    :for pair :in (parse-input input)
    :sum (apply #'battle-score pair)))

(defun letter-to-outcome (letter)
  (case (a:make-keyword letter)
    (:x :lose)
    (:y :draw)
    (:z :win)))

(defun opponent-and-outcome-to-you (opponent outcome)
  (case outcome
    (:lose (case opponent
             (:rock :scissors)
             (:paper :rock)
             (:scissors :paper)))
    (:draw opponent)
    (:win (case opponent
            (:rock :paper)
            (:paper :scissors)
            (:scissors :rock)))))

(defun parse-input-2 (input)
  (loop
    :with split := (uiop:split-string input :separator '(#\space #\newline))
    :for (opponent outcome) :on split :by #'cddr
    :collect (list (letter-to-item opponent)
                   (letter-to-outcome outcome))))

(defun problem-2 (&key (input *input-part-2-test*))
  (loop
    :for (opponent outcome) :in (parse-input-2 input)
    :for you := (opponent-and-outcome-to-you opponent outcome)
    :sum (battle-score opponent you)))

(defparameter *input-part-1-test*
  "A Y
B X
C Z")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  (u:get-problem-data 2022 2))
