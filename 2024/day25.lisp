(in-package #:advent-of-code-2024.day25)


(defun parse-input (input)
  (let* ((raw (loop
                for lock in (str:split "

" input)
                collect (util:parse-string-into-array lock)))
         (locks (extract-locks raw))
         (keys (extract-keys raw)))
    (list locks keys (- (array-dimension (first raw) 0) 2))))

(defun extract-locks (data)
  (flet ((lock? (arr)
           (loop
             for col from 0 below (array-dimension arr 1)
             always (char= #\# (aref arr 0 col))))
         (process (arr)
           (let ((res (make-array (list (array-dimension arr 1))
                                  :initial-element 0)))
             (util:array-loop (arr (r c) :item el)
               (when (and (plusp r) (char= #\# el))
                 (incf (aref res c))))
             res)))
    (loop
      for item in data
      when (lock? item)
        collect (process item))))

(defun extract-keys (data)
  (let ((rows (array-dimension (first data) 0)))
   (flet ((key? (arr)
            (loop
              for col from 0 below (array-dimension arr 1)
              always (char= #\# (aref arr (1- rows) col))))
          (process (arr)
            (let ((res (make-array (list (array-dimension arr 1))
                                   :initial-element 0)))
              (util:array-loop (arr (r c) :item el)
                (when (and (< r (1- rows)) (char= #\# el))
                  (incf (aref res c))))
              res)))
     (loop
       for item in data
       when (key? item)
         collect (process item)))))

(defun problem-1 (&key (input *input-part-1-test*))
  (destructuring-bind (locks keys full-height)
      (parse-input input)
    (loop
      for l in locks
      sum (loop
            for k in keys
            count (pair-fits? k l full-height)))))

(defun pair-fits? (k l full-height)
  (loop
    for c from 0 below (array-dimension k 0)
    always (>= full-height (+ (aref k c) (aref l c)))))

(defparameter *input-part-1-test*
  "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  ".....
..#..
..#..
..#..
#.#.#
#.###
#####

#####
#####
##.##
##.##
#..##
....#
.....

#####
#####
.#.##
....#
....#
....#
.....

#####
####.
.###.
..##.
...#.
...#.
.....

#####
####.
####.
####.
.#.#.
.#.#.
.....

#####
####.
.###.
..##.
...#.
.....
.....

#####
##.##
#..##
#...#
....#
.....
.....

#####
#####
.####
..##.
..#..
.....
.....

.....
.....
.....
..#.#
.##.#
#####
#####

.....
.....
.....
....#
#.#.#
#####
#####

#####
####.
##.#.
##...
##...
#....
.....

.....
#..#.
#..#.
#..#.
#.##.
####.
#####

.....
...#.
.#.#.
##.#.
##.#.
##.#.
#####

#####
#####
#####
###.#
#.#.#
..#.#
.....

#####
###.#
###.#
###.#
.#..#
.#...
.....

#####
###.#
.##.#
.##.#
.##..
..#..
.....

.....
....#
.#..#
.#..#
.#..#
.##.#
#####

.....
.....
#.#..
#.#.#
#####
#####
#####

.....
.....
..#..
#.#..
#.#..
#.##.
#####

.....
....#
.#..#
.#.##
.#.##
.#.##
#####

.....
...#.
...##
.#.##
##.##
#####
#####

#####
#.#.#
#...#
#...#
#...#
.....
.....

#####
#####
.####
.####
.##.#
.#...
.....

#####
#.###
#.##.
...#.
.....
.....
.....

.....
.#.#.
.###.
.###.
.###.
####.
#####

#####
#####
#####
##.#.
#..#.
...#.
.....

#####
##.##
#..##
#..##
#..#.
#..#.
.....

.....
#.#..
#.#..
#.##.
#.##.
#.###
#####

.....
.#..#
##..#
###.#
###.#
#####
#####

.....
.#...
.#...
.#.#.
.#.#.
.#.##
#####

#####
##.##
#...#
....#
....#
....#
.....

#####
#####
#####
#.#.#
#.#..
.....
.....

.....
..#..
..#..
..#..
..##.
#.##.
#####

.....
...#.
...##
...##
#..##
##.##
#####

#####
#####
##.##
#..##
#...#
.....
.....

#####
.#.##
.#.##
....#
....#
.....
.....

.....
...#.
...#.
...##
.#.##
#####
#####

#####
###.#
#.#..
#.#..
..#..
.....
.....

#####
#.###
...##
...##
...#.
...#.
.....

.....
.....
..#..
..#..
#.#..
#.#.#
#####

#####
####.
.##..
.##..
.##..
.#...
.....

.....
.....
#....
#....
#..#.
#.###
#####

#####
#####
#.###
..##.
...#.
.....
.....

.....
#...#
##..#
###.#
#####
#####
#####

#####
#####
#####
.#.##
.#.##
.#..#
.....

.....
.....
.#.#.
.#.#.
.#.#.
.###.
#####

.....
....#
.#.##
.#.##
.#.##
##.##
#####

#####
#####
#.##.
#.#..
#.#..
#.#..
.....

.....
.....
.#...
.#.#.
##.#.
##.#.
#####

#####
#####
#####
####.
##.#.
.#...
.....

.....
#...#
#.#.#
#.#.#
#.#.#
#.###
#####

.....
...#.
.#.#.
##.#.
####.
#####
#####

#####
#####
#####
.###.
.#.#.
.....
.....

.....
#.#..
#.#..
###..
###..
###.#
#####

#####
#.###
..##.
...#.
...#.
.....
.....

.....
#.#..
#.##.
#.##.
#.##.
####.
#####

#####
####.
##.#.
.#...
.....
.....
.....

.....
....#
#..##
#..##
#.###
#.###
#####

.....
.....
.....
...#.
.#.##
#####
#####

#####
.####
.####
.####
..###
..#.#
.....

#####
#####
#####
###.#
#.#..
..#..
.....

.....
.....
#...#
##..#
###.#
#####
#####

#####
#.###
#..##
#..#.
#..#.
.....
.....

.....
#.#..
#.#..
#.##.
#.###
#####
#####

#####
#####
##.##
#...#
....#
.....
.....

#####
####.
##.#.
##.#.
#....
#....
.....

.....
..#..
..#..
.###.
####.
#####
#####

#####
.####
.####
..#.#
..#..
..#..
.....

#####
###.#
#.#.#
....#
.....
.....
.....

.....
.....
.....
.....
..#.#
#.###
#####

#####
.####
.#.##
.#..#
.#...
.....
.....

.....
#....
#.#..
####.
####.
####.
#####

.....
.....
#.#..
####.
####.
#####
#####

#####
#####
##.##
##.##
##.##
.#..#
.....

.....
..#.#
..#.#
..#.#
..#.#
.####
#####

#####
#####
.#.##
....#
.....
.....
.....

#####
#.###
..##.
..##.
..#..
..#..
.....

#####
#####
##.#.
##.#.
.#.#.
...#.
.....

.....
..#.#
..#.#
..#.#
.##.#
#####
#####

.....
.....
..#..
.##..
####.
#####
#####

#####
#.#.#
#.#.#
#.#..
#.#..
#.#..
.....

.....
.....
....#
.#.##
##.##
##.##
#####

#####
.####
.#.#.
.#.#.
.#.#.
.....
.....

#####
#####
##.##
.#.#.
.#.#.
.#.#.
.....

#####
#.###
#.#.#
#.#.#
....#
.....
.....

#####
#.##.
#..#.
...#.
.....
.....
.....

.....
.#...
.#.#.
##.#.
#####
#####
#####

#####
##.##
##.##
##.##
##.#.
.#...
.....

#####
#####
.#.##
...##
...#.
...#.
.....

#####
#####
###.#
##..#
##..#
#....
.....

.....
#...#
#.#.#
#.#.#
###.#
#####
#####

.....
.....
..#.#
.##.#
###.#
#####
#####

.....
.....
#.#..
#.#..
#.#.#
#.###
#####

#####
#####
##.##
.#.##
.#.#.
.#.#.
.....

.....
#..#.
#..#.
#..#.
##.##
##.##
#####

.....
.#.#.
.#.#.
####.
#####
#####
#####

#####
#####
###.#
###.#
.##.#
.#..#
.....

.....
.....
#.#.#
#.#.#
#.###
#.###
#####

#####
##.##
##.##
.#..#
.#...
.....
.....

#####
##.#.
##.#.
##.#.
#..#.
#....
.....

.....
.#...
.#...
.#...
##.#.
#####
#####

#####
##.#.
.#.#.
.#...
.....
.....
.....

.....
.....
....#
#..##
#.###
#####
#####

.....
....#
..#.#
.##.#
.####
.####
#####

.....
..#.#
..#.#
#.###
#####
#####
#####

#####
####.
####.
####.
#.#..
.....
.....

.....
.....
.....
..#..
.###.
.###.
#####

.....
.#...
.#..#
##..#
##.##
##.##
#####

#####
#.##.
#.#..
..#..
..#..
..#..
.....

.....
..#.#
..###
..###
#.###
#.###
#####

.....
.#...
.#..#
.#..#
##..#
###.#
#####

.....
..#..
.##..
.##..
.##..
.##.#
#####

#####
#####
#.###
#.##.
..##.
..#..
.....

.....
.....
..#..
..#.#
..#.#
.####
#####

#####
#####
####.
#.#..
#.#..
..#..
.....

.....
#..#.
#..##
##.##
##.##
##.##
#####

#####
####.
.###.
.###.
..##.
...#.
.....

.....
..#..
..#.#
.##.#
###.#
#####
#####

.....
#.#..
#.#..
#.#.#
#.#.#
#.#.#
#####

.....
.#...
.#...
.#..#
###.#
###.#
#####

.....
....#
....#
.#.##
.####
#####
#####

#####
#.###
#.###
#.###
..#.#
.....
.....

.....
....#
....#
....#
.#..#
.##.#
#####

#####
.#.#.
.#.#.
.#.#.
.#.#.
.#...
.....

#####
###.#
###..
.##..
.##..
..#..
.....

#####
#.#.#
#.#.#
#.#.#
..#.#
.....
.....

.....
.#...
##.#.
##.##
##.##
##.##
#####

.....
.#..#
.#..#
.#..#
##.##
#####
#####

#####
.####
.####
.##.#
.##.#
.#...
.....

#####
###.#
###..
.#...
.#...
.#...
.....

#####
###.#
#.#..
..#..
..#..
..#..
.....

#####
##.##
.#.#.
.#...
.....
.....
.....

.....
.#...
.#...
.#.#.
.#.##
##.##
#####

.....
.#...
.#..#
###.#
###.#
#####
#####

#####
####.
####.
##.#.
#..#.
#....
.....

#####
##.#.
##...
.#...
.#...
.#...
.....

.....
..#.#
..#.#
.##.#
.##.#
###.#
#####

.....
.....
..#..
..##.
#.###
#.###
#####

.....
..#..
..#.#
..#.#
..#.#
.##.#
#####

.....
...#.
..##.
.####
#####
#####
#####

.....
#..#.
#.###
#.###
#.###
#####
#####

.....
..#..
.##..
.###.
####.
#####
#####

.....
.....
....#
...##
.#.##
.#.##
#####

.....
...#.
...#.
...#.
..###
#.###
#####

.....
.#...
.##.#
.##.#
.####
#####
#####

#####
#####
##.##
##.##
##..#
#....
.....

#####
#####
#.##.
..#..
.....
.....
.....

#####
#.###
#.##.
#.#..
..#..
.....
.....

.....
#....
#..#.
#..##
#.###
#####
#####

.....
.#..#
##.##
##.##
##.##
##.##
#####

#####
#####
##.##
##..#
##...
.#...
.....

#####
#####
#.###
#.#.#
#.#.#
#...#
.....

#####
#####
###.#
.##.#
.#..#
.#...
.....

#####
.#.##
.#.##
.#.##
...##
...#.
.....

#####
##.#.
##.#.
#..#.
#..#.
.....
.....

#####
#####
###.#
###.#
###.#
#.#..
.....

.....
.....
.#.#.
.#.#.
.#.#.
.#.##
#####

#####
#####
#####
###.#
.##.#
..#.#
.....

.....
...#.
..##.
.###.
#####
#####
#####

.....
....#
....#
....#
#.#.#
#.###
#####

.....
....#
#.#.#
###.#
###.#
###.#
#####

#####
.###.
.###.
.#.#.
.#...
.....
.....

.....
..#..
..#..
..##.
..###
#.###
#####

.....
#.#..
#.#..
###..
####.
#####
#####

.....
.....
#....
#....
#....
#.#.#
#####

.....
.....
.....
.....
#..#.
##.#.
#####

#####
##.##
##.##
##.##
.#..#
.#..#
.....

.....
.#..#
.#.##
##.##
#####
#####
#####

#####
#####
#####
#.#.#
#.#.#
#.#.#
.....

#####
#####
##.##
##.##
#...#
#...#
.....

.....
.#...
.#...
.##..
.###.
#####
#####

#####
#####
#####
.###.
..##.
..#..
.....

.....
....#
.#..#
.#..#
.#.##
#####
#####

#####
#.###
#.###
#..#.
#....
#....
.....

.....
.....
..#..
..#..
..#.#
#.###
#####

.....
.....
.#...
.#..#
###.#
#####
#####

#####
.#.##
.#.##
.#.##
.#.#.
...#.
.....

.....
.....
.#..#
##..#
##.##
##.##
#####

.....
.#...
.##..
.##..
####.
#####
#####

.....
.....
.#...
##..#
##.##
#####
#####

#####
###.#
###.#
.#...
.#...
.#...
.....

#####
#####
#####
####.
.#.#.
...#.
.....

.....
...#.
.#.##
##.##
##.##
#####
#####

#####
#.###
#..#.
#....
#....
.....
.....

#####
#####
#####
#####
##.#.
#..#.
.....

.....
..#..
..#..
..##.
#.##.
#####
#####

.....
#....
#....
#..#.
##.##
##.##
#####

.....
.....
...#.
...#.
#.##.
#.###
#####

#####
####.
###..
#.#..
#....
.....
.....

.....
..#..
#.#..
#.#.#
###.#
#####
#####

#####
#.###
..#.#
..#..
.....
.....
.....

#####
#####
####.
####.
#.#..
#.#..
.....

.....
#....
#....
##...
###.#
#####
#####

#####
#.###
#..#.
...#.
...#.
.....
.....

#####
#.###
..#.#
....#
.....
.....
.....

.....
.....
..#..
#.#.#
###.#
###.#
#####

#####
#####
###.#
#.#.#
....#
.....
.....

#####
#.###
#.###
#.#.#
#.#.#
.....
.....

.....
....#
#.#.#
#.#.#
###.#
#####
#####

#####
#.###
..###
..##.
...#.
.....
.....

.....
#..#.
##.#.
##.#.
##.#.
#####
#####

#####
#.##.
#.#..
#.#..
#.#..
..#..
.....

.....
.#..#
.##.#
.##.#
#####
#####
#####

.....
.....
...#.
...#.
#.##.
#####
#####

.....
...#.
...#.
...#.
.#.#.
.#.##
#####

.....
#....
#....
#....
##.#.
##.#.
#####

.....
....#
....#
..#.#
#.#.#
###.#
#####

#####
#.###
..#.#
..#.#
.....
.....
.....

#####
.###.
.#.#.
.#.#.
...#.
...#.
.....

#####
#####
#####
##.##
#..##
....#
.....

#####
####.
#.#..
..#..
.....
.....
.....

.....
.....
.....
..#..
..#..
.##.#
#####

.....
.....
.....
..#..
.###.
####.
#####

#####
.##.#
.#..#
.#...
.#...
.....
.....

#####
##.##
##.##
##.##
#..#.
#..#.
.....

.....
...#.
..###
..###
.####
.####
#####

#####
#####
###.#
###..
#.#..
..#..
.....

.....
...#.
...#.
#.###
#.###
#####
#####

.....
#....
#....
#....
##...
##.#.
#####

.....
.....
.....
...#.
.#.#.
.#.#.
#####

.....
.....
.....
..#..
.##..
####.
#####

#####
###.#
##..#
#...#
....#
.....
.....

#####
#####
#.###
..##.
..#..
..#..
.....

.....
.#...
.#...
.##..
.###.
.####
#####

.....
.#...
.#...
.#.#.
##.#.
#####
#####

#####
#.#.#
#.#..
..#..
.....
.....
.....

#####
##.##
##.##
##.#.
##.#.
#..#.
.....

.....
.#...
.#...
##.#.
##.#.
####.
#####

.....
....#
....#
.#..#
.#.##
#####
#####

#####
#####
#####
##.##
##.#.
.#...
.....

#####
#####
.#.#.
...#.
...#.
...#.
.....

.....
...#.
..###
#.###
#####
#####
#####

.....
.....
..#..
.###.
.###.
#####
#####

.....
....#
....#
..#.#
.####
.####
#####

#####
###.#
#.#.#
#.#.#
....#
.....
.....

.....
.#...
.##..
.##..
###.#
###.#
#####

.....
.#.#.
.###.
.###.
.####
#####
#####

.....
....#
..#.#
..###
.####
.####
#####

#####
###.#
###.#
###.#
.#...
.#...
.....

#####
###.#
###..
##...
#....
.....
.....

.....
.....
..#..
..#..
#.#..
####.
#####

.....
...#.
#.##.
#.##.
#.###
#.###
#####

#####
##.##
##.##
.#.#.
...#.
.....
.....

#####
.####
.####
.####
.#.##
....#
.....

.....
..#.#
..#.#
..#.#
.####
#####
#####

#####
##.##
##.##
##..#
##..#
#...#
.....

.....
.#...
.#..#
.#..#
###.#
###.#
#####

.....
.....
...#.
...#.
.#.#.
.#.##
#####

#####
####.
####.
###..
###..
#.#..
.....

#####
#####
.##.#
.##..
.#...
.....
.....

.....
..#.#
..#.#
..#.#
..###
.####
#####

#####
#####
#####
###.#
###.#
.#...
.....

#####
#.###
#.#.#
#...#
#...#
.....
.....

.....
..#..
#.#.#
#.###
#####
#####
#####

#####
##.#.
##.#.
#..#.
.....
.....
.....

#####
.####
.####
..#.#
..#.#
..#.#
.....

#####
.#.##
.#.##
.#..#
.#..#
.#..#
.....

.....
.....
....#
#...#
##..#
##.##
#####

.....
.#...
.#...
##.#.
####.
####.
#####

#####
#.##.
#.#..
..#..
..#..
.....
.....

#####
#####
##.##
##.#.
##...
#....
.....

.....
...#.
...#.
...#.
#..#.
#.##.
#####

.....
.....
.....
.#.#.
.#.##
##.##
#####

#####
#####
#.##.
#.##.
#.##.
#..#.
.....

.....
.#.#.
.#.#.
.#.##
.#.##
##.##
#####

#####
#.#.#
#...#
#...#
#...#
#...#
.....

#####
.###.
.###.
.###.
..##.
...#.
.....

#####
#.###
#.###
#.#.#
#.#..
#.#..
.....

.....
.....
#.#.#
###.#
#####
#####
#####

.....
#..#.
#..#.
#..##
##.##
#####
#####

.....
.#.#.
.#.##
#####
#####
#####
#####

#####
###.#
##..#
#...#
.....
.....
.....

.....
..#..
.##..
.##..
.##..
###.#
#####

.....
.#...
###.#
###.#
###.#
#####
#####

#####
.####
.###.
..##.
..##.
..#..
.....

.....
.....
.....
.#.#.
.#.##
.####
#####

.....
#..#.
#.##.
#.###
#.###
#.###
#####

#####
#####
#.##.
..##.
..#..
.....
.....

.....
.....
.....
.#..#
##..#
##.##
#####

#####
#.#.#
..#.#
..#..
..#..
..#..
.....

.....
.....
.....
.....
...#.
.#.#.
#####

.....
.....
.#...
###.#
###.#
#####
#####

#####
.####
.#.#.
...#.
.....
.....
.....

#####
####.
##.#.
##.#.
.#...
.#...
.....

.....
#....
##.#.
##.#.
####.
####.
#####

#####
#####
.##.#
.##.#
.#..#
.#..#
.....

#####
##.#.
##.#.
##.#.
##.#.
#....
.....

#####
#####
#####
##.##
.#.##
...#.
.....

#####
.####
.####
..##.
..##.
...#.
.....

#####
###.#
###.#
###.#
#.#.#
#.#.#
.....

.....
.#...
.#...
.#.#.
.#.#.
##.##
#####

.....
#...#
##..#
###.#
###.#
###.#
#####

#####
#####
#####
#.#.#
..#.#
..#.#
.....

#####
####.
##.#.
#..#.
...#.
.....
.....

.....
.#.#.
.#.#.
.#.#.
.#.#.
#####
#####

.....
..#.#
..#.#
..#.#
#.###
#.###
#####

.....
.....
.....
.#.#.
####.
#####
#####

.....
...#.
...#.
...##
..###
.####
#####

.....
..#..
.##..
.##.#
###.#
#####
#####

#####
.##.#
.#...
.#...
.#...
.#...
.....

#####
#####
.##.#
.##.#
.##..
..#..
.....

.....
.#...
.#..#
##.##
#####
#####
#####

#####
###.#
###.#
##..#
#...#
....#
.....

#####
#####
.###.
.##..
.#...
.....
.....

.....
#....
#....
##..#
##..#
##.##
#####

#####
#####
####.
####.
####.
.#.#.
.....

.....
..#..
..#..
..#..
..##.
.###.
#####

#####
#####
#####
###.#
##..#
#...#
.....

.....
.....
.....
#..#.
#..#.
#.###
#####

#####
#.###
#.###
#..##
#..##
....#
.....

#####
.#.##
.#.##
.#..#
.#..#
....#
.....

.....
....#
....#
.#..#
.##.#
###.#
#####

.....
.#...
.#.#.
.###.
.####
#####
#####

.....
.#...
.#...
##...
##..#
##.##
#####

.....
.....
.....
#....
#..#.
#.###
#####

#####
#####
#####
##.##
.#.#.
.#.#.
.....

.....
#..#.
##.##
#####
#####
#####
#####

#####
#####
##.#.
##...
#....
#....
.....

.....
#....
#.#.#
#.#.#
#.###
#####
#####

#####
##.##
#..##
#..##
....#
.....
.....

.....
.....
...#.
#..#.
#.##.
#.##.
#####

#####
.####
.#.##
.#.#.
.#.#.
.#.#.
.....

.....
#....
#....
##..#
##.##
#####
#####

.....
#...#
#...#
#...#
##..#
###.#
#####

#####
###.#
###..
##...
#....
#....
.....

#####
#####
##.##
.#.#.
.....
.....
.....

#####
###.#
.##..
..#..
..#..
..#..
.....

.....
...#.
.#.#.
.###.
#####
#####
#####

.....
#....
#....
#..#.
#..#.
#.##.
#####

#####
##.##
#..##
#..##
...##
...#.
.....

#####
##.##
##.#.
#..#.
...#.
...#.
.....

#####
####.
####.
.#.#.
.#.#.
...#.
.....

#####
.###.
.###.
.##..
..#..
..#..
.....

.....
.....
....#
.#..#
##.##
#####
#####

.....
.....
..#.#
..#.#
#.#.#
#.#.#
#####

#####
#.###
#.###
..#.#
..#.#
.....
.....

#####
#####
#.###
...#.
...#.
...#.
.....

.....
.....
.....
..#..
#.#.#
#.###
#####

#####
#####
#.###
#.#.#
#.#..
..#..
.....

#####
#####
##.#.
##.#.
.#.#.
.#...
.....

.....
.#...
.#...
##...
##.#.
##.#.
#####

.....
#....
#..#.
#..#.
##.#.
##.#.
#####

.....
..#..
..#.#
#.###
#####
#####
#####

#####
#####
#####
#####
#.#.#
#.#.#
.....

.....
.....
.....
..#..
..#..
#.##.
#####

.....
...#.
#..#.
#..##
#..##
##.##
#####

#####
.####
.#.#.
.#.#.
.#...
.#...
.....

#####
#.#.#
#.#..
..#..
..#..
.....
.....

#####
#.###
#.###
#.#.#
....#
....#
.....

#####
###.#
###..
###..
##...
#....
.....

.....
.#..#
.##.#
.####
.####
#####
#####

#####
###.#
###.#
###..
#.#..
#....
.....

.....
.#..#
.#..#
##.##
##.##
#####
#####

#####
#####
#.###
#..#.
...#.
.....
.....

#####
##.##
.#.##
.#.#.
.#.#.
.....
.....

.....
.....
..#..
..#..
..##.
#.##.
#####

.....
#..#.
#..##
#..##
##.##
#####
#####

.....
.....
.#...
##..#
###.#
###.#
#####

.....
.....
#.#..
###..
###..
###.#
#####

.....
.#.#.
.#.#.
.#.#.
.#.#.
.####
#####

.....
.....
....#
...##
...##
.#.##
#####

.....
#.#..
#.#..
#.#..
#.#.#
#####
#####

#####
#####
.####
.##.#
.##.#
.#...
.....

#####
#####
#####
##.##
#..##
#..#.
.....

#####
####.
.#.#.
.#.#.
.....
.....
.....

#####
#.##.
#.##.
#.##.
#.#..
.....
.....

.....
.....
...#.
..##.
.###.
####.
#####

#####
#####
#.##.
#.#..
#.#..
.....
.....

.....
.....
.....
.#...
.#.#.
.#.#.
#####

.....
.#...
.#...
##..#
###.#
#####
#####

#####
#####
#.#.#
#.#.#
#.#..
#.#..
.....

#####
.####
.####
..###
..###
..#.#
.....

#####
.##.#
..#.#
.....
.....
.....
.....

.....
.....
..#..
..##.
..##.
.####
#####

#####
#####
.##.#
..#.#
....#
.....
.....

#####
#.###
#..##
#..##
#..#.
...#.
.....

#####
#####
##.##
.#.##
.#.#.
.#...
.....

#####
##.#.
##.#.
##...
.#...
.....
.....

.....
.....
.....
....#
..#.#
.##.#
#####

#####
#####
#####
.####
.####
.#.#.
.....

#####
#.#.#
#.#.#
#.#.#
#.#.#
#...#
.....

#####
#.###
#..#.
#..#.
#..#.
.....
.....

#####
.##.#
.##.#
..#.#
..#..
..#..
.....

.....
.....
..#.#
..#.#
#.#.#
#####
#####

.....
.#.#.
.#.#.
.#.#.
####.
#####
#####

.....
#....
#...#
#.#.#
#####
#####
#####

#####
.####
.####
..##.
...#.
.....
.....

.....
#....
#....
#....
#.#..
#.##.
#####

.....
.#...
##...
###..
###..
###.#
#####

#####
####.
.###.
.###.
.#.#.
.....
.....

#####
.####
.##.#
.##.#
..#.#
..#..
.....

#####
####.
##.#.
##.#.
#..#.
.....
.....

.....
.....
.....
.....
.#.#.
##.#.
#####

#####
#####
##.##
##.##
.#.##
.#..#
.....

#####
###.#
#.#.#
#.#.#
..#.#
..#.#
.....

#####
#.#.#
#...#
#...#
#...#
....#
.....

.....
....#
#...#
#...#
#.#.#
#.###
#####

.....
#...#
#...#
#...#
##.##
#####
#####

#####
#.###
#.###
#..##
#...#
#...#
.....

.....
..#..
#.#..
#.#..
#.#..
#.##.
#####

.....
.....
#....
#.#.#
#.#.#
#####
#####

#####
#.#.#
#...#
....#
.....
.....
.....

.....
#....
#..#.
#..#.
#..##
##.##
#####

.....
.#...
.#...
.##.#
.####
#####
#####

.....
...#.
...#.
#.###
#.###
#.###
#####

.....
.....
#..#.
#..##
#.###
#.###
#####

.....
.....
.#..#
.#..#
.#.##
#####
#####

.....
.....
..#..
#.#.#
#.#.#
#.###
#####

#####
#####
####.
#.##.
..#..
..#..
.....

#####
#####
####.
####.
#.##.
#..#.
.....

#####
.####
.#.#.
.#.#.
.....
.....
.....

.....
..#.#
#.#.#
###.#
###.#
###.#
#####

#####
####.
#.##.
#.#..
#.#..
..#..
.....

#####
#.##.
#.##.
#.#..
..#..
.....
.....

.....
#....
#...#
#..##
##.##
##.##
#####

#####
#.###
..#.#
..#..
..#..
..#..
.....

.....
.#...
.#..#
##.##
##.##
#####
#####

#####
#####
#####
##.##
##..#
.#..#
.....

.....
.....
.....
.....
...#.
#.###
#####

#####
.#.##
.#.##
.#.##
.#.##
.#..#
.....

#####
.####
..##.
...#.
.....
.....
.....

.....
#....
#....
#.#..
###.#
#####
#####

#####
###.#
.##.#
.##.#
.#..#
....#
.....

#####
#.###
#.##.
#.##.
#.##.
...#.
.....

#####
.##.#
.##..
.##..
..#..
.....
.....

.....
.....
..#.#
.####
.####
.####
#####

.....
...#.
...#.
#..#.
#..##
##.##
#####

#####
#.###
#..##
#...#
....#
.....
.....

#####
####.
####.
#.##.
#.##.
#..#.
.....

#####
#.###
#.###
..#.#
..#.#
..#.#
.....

#####
#####
##.#.
.#.#.
...#.
...#.
.....

.....
.#...
.#.#.
####.
#####
#####
#####

.....
.#...
##.#.
#####
#####
#####
#####

#####
.##.#
.##.#
.##..
.#...
.....
.....

.....
#...#
#...#
#.#.#
#.#.#
###.#
#####

.....
..#..
..#..
..#..
.##.#
.####
#####

#####
###.#
.#...
.#...
.....
.....
.....

.....
.....
#.#.#
#.#.#
#.#.#
#.###
#####

.....
..#..
..#..
#.##.
#####
#####
#####

#####
###.#
.##..
.##..
.##..
..#..
.....

.....
...#.
..###
..###
.####
#####
#####

#####
###.#
.##..
.##..
..#..
.....
.....

#####
#.###
#.###
#..##
#...#
....#
.....

#####
.###.
.###.
.#.#.
...#.
...#.
.....

#####
#####
##.##
##..#
.#..#
.....
.....

#####
####.
####.
#.##.
#.##.
...#.
.....

.....
.....
#....
##..#
##..#
###.#
#####

#####
##.##
##.##
##..#
.#...
.#...
.....

.....
..#.#
..#.#
.##.#
.####
#####
#####

#####
#.###
#.###
#.#.#
#.#.#
#....
.....

#####
.####
.####
.####
.##.#
.#...
.....

.....
...#.
...#.
...##
.#.##
##.##
#####

#####
.####
..#.#
....#
....#
....#
.....

.....
.#.#.
.#.##
.#.##
##.##
#####
#####

#####
###.#
##..#
.#..#
.#...
.#...
.....

.....
.#...
.#.#.
##.##
##.##
##.##
#####

.....
....#
..#.#
..###
#.###
#####
#####

#####
#####
#####
##.#.
##...
#....
.....

#####
#####
#####
#####
###.#
#.#..
.....

.....
.....
#..#.
#..#.
#..#.
#.##.
#####

#####
#.###
#.##.
..##.
...#.
...#.
.....

.....
.....
.....
...#.
..##.
.####
#####

#####
#####
.#.##
.#..#
.....
.....
.....

#####
.##.#
..#.#
..#.#
..#.#
..#..
.....

#####
##.#.
##.#.
#..#.
#....
.....
.....

.....
.....
....#
.#..#
.#..#
.##.#
#####

.....
.....
#.#..
####.
#####
#####
#####

#####
#####
#####
##.##
.#.##
.#.#.
.....

.....
.#...
###..
###.#
###.#
###.#
#####

#####
#.###
..###
..###
...##
...#.
.....

.....
..#..
.###.
.###.
####.
####.
#####

.....
.#...
.#...
.##..
###..
####.
#####

#####
.###.
.###.
.##..
..#..
.....
.....

#####
#.###
#.##.
#.##.
...#.
...#.
.....

.....
.#.#.
.###.
.###.
.###.
#####
#####

.....
....#
#...#
##..#
###.#
#####
#####

.....
.....
..#..
..#.#
..###
.####
#####

.....
..#..
..#..
#.#.#
#.#.#
#####
#####

.....
...#.
#..#.
##.#.
####.
#####
#####

#####
#.###
#.###
#.###
..##.
..#..
.....

.....
.....
..#..
..#..
#.#..
###.#
#####

.....
#....
#...#
#...#
##..#
###.#
#####

#####
##.#.
.#.#.
...#.
...#.
.....
.....

.....
....#
.#..#
.#..#
.#.##
##.##
#####

#####
.####
.####
.#.##
.#.#.
.....
.....

#####
.###.
..##.
..##.
...#.
...#.
.....

#####
##.##
#..##
#..#.
...#.
...#.
.....

.....
.#...
.##..
.##.#
###.#
#####
#####

#####
#####
####.
#.##.
..##.
...#.
.....

#####
#####
###.#
###.#
.#...
.....
.....

.....
.....
...#.
..##.
.###.
.####
#####

.....
....#
....#
.#..#
##..#
###.#
#####

.....
#...#
#...#
#.#.#
#####
#####
#####

#####
.####
.####
.#.#.
.#.#.
.....
.....

#####
#.###
#.###
#.#.#
..#.#
.....
.....

.....
#....
##...
##.#.
##.#.
##.#.
#####

.....
.....
....#
....#
#.#.#
###.#
#####

.....
....#
....#
....#
....#
#.#.#
#####

.....
.....
.....
#.#..
#.#.#
#.#.#
#####

#####
##.##
.#.#.
.#.#.
.#.#.
.#...
.....")
