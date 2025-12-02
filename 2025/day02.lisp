(in-package #:advent-of-code-2025.day02)

(defun parse-input (input)
  (loop
    :for range :in (uiop:split-string input :separator '(#\,))
    :collect (util:parse-integers range :sep '(#\-))))

(defun invalid? (num)
  (let ((ndigits (util:digit-count num)))
    (and (evenp ndigits)
         (multiple-value-bind (l r)
             (truncate num (expt 10 (/ ndigits 2)))
           (= l r)))))

(defun problem1 (&key (input *input-part-1-test*))
  (loop
    :for (low high) :in (parse-input input)
    :sum (loop
           :for num :from low :to high
           :when (invalid? num)
             :sum num)))

(defun generated-by-sub? (num factor)
  (loop
    :with d := (expt 10 factor)
    :for n := num :then q
    :while (plusp n)
    :for (q r) := (multiple-value-list (truncate n d))
    :collect r :into rems
    :finally (return (= 1 (length (delete-duplicates rems))))))

(defun invalid>=2? (num)
  (loop
    :for factor :in (cons 1 (util:factors (util:digit-count num)))
    :when (generated-by-sub? num factor)
      :do (return t)))

(defun problem2 (&key (input *input-part-1-test*))
  (loop
    :for (low high) :in (parse-input input)
    :sum (loop
           :for num :from low :to high
           :when (and (>= num 10) (invalid>=2? num))
             :sum num)))

(defparameter *input-part-1-test*
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defparameter *input-part-2-test* *input-part-1-test*)

(defparameter *input*
  "1061119-1154492,3-23,5180469-5306947,21571-38630,1054-2693,141-277,2818561476-2818661701,21177468-21246892,40-114,782642-950030,376322779-376410708,9936250-10074071,761705028-761825622,77648376-77727819,2954-10213,49589608-49781516,9797966713-9797988709,4353854-4515174,3794829-3861584,7709002-7854055,7877419320-7877566799,953065-1022091,104188-122245,25-39,125490-144195,931903328-931946237,341512-578341,262197-334859,39518-96428,653264-676258,304-842,167882-252124,11748-19561")
