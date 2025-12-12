(in-package #:advent-of-code-2025.day11)

(defun parse-input (input)
  (loop
    :with h := (make-hash-table)
    :for line :in (u:split-lines input)
    :for (input . outputs) := (mapcar #'string-upcase (re:split ":? " line))
    :for in := (intern input)
    :do (loop
          :for out :in outputs
          :do (push (intern out) (gethash in h)))
    :finally (return h)))

(function-cache:defcached paths (src graph)
  (if (eq src 'out)
      1
      (loop
        :for dst :in (gethash src graph)
        :sum (paths dst graph))))

(defun problem1 (&key (input *input-part-1-test*))
  (let ((graph (parse-input input)))
    (paths 'you graph)))

(function-cache:defcached paths2 (src graph &key dac-seen fft-seen)
  (if (eq src 'out)
      (if (and dac-seen fft-seen) 1 0)
      (loop
        :for dst :in (gethash src graph)
        :sum (paths2 dst graph
                     :dac-seen (or dac-seen (eq dst 'dac))
                     :fft-seen (or fft-seen (eq dst 'fft))))))

(defun problem2 (&key (input *input-part-2-test*))
  (let ((graph (parse-input input)))
    (paths2 'svr graph)))

(defparameter *input-part-1-test*
  "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

(defparameter *input-part-2-test*
  "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

(defparameter *input*
  (u:get-problem-data 2025 11))
