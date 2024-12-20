(in-package #:advent-of-code-2024.day20)

(defun parse-input (input)
  (let ((arr (util:parse-string-into-array input))
        start
        end)
    (util:array-loop (arr (r c) :item el)
                     (when (char= #\S el)
                       (setf start `(,r ,c))
                       (setf (aref arr r c) #\.))
                     (when (char= #\E el)
                       (setf end `(,r ,c))
                       (setf (aref arr r c) #\.)))
    (values arr start end)))

(defparameter *deltas* '((1 0) (0 1) (-1 0) (0 -1)))

(defun neighbors (arr r c)
  (loop
    for (dr dc) in *deltas*
    when (ignore-errors (char= #\. (aref arr (+ r dr) (+ c dc))))
      collect `(,(+ r dr) ,(+ c dc))))

(defun find-distances (arr start)
  (destructuring-bind (sr sc) start
   (let ((q (s:queue start))
         (h (a:alist-hash-table `(((,sr ,sc) . 0)) :test #'equal)))
     (loop
       until (s:queue-empty-p q)
       for (r c) = (s:deq q)
       do (loop
            for (nr nc) in (neighbors arr r c)
            unless (gethash `(,nr ,nc) h)
              do (progn
                   (setf (gethash `(,nr ,nc) h)
                         (+ 1 (gethash `(,r ,c) h)))
                   (s:enq `(,nr ,nc) q)))
       )
     h)))

(defun problem-1 (&key (input *input-part-1-test*))
  (multiple-value-bind (arr start end)
      (parse-input input)
    (let ((d (find-distances arr start)))
      (loop
        for (r1 c1) being the hash-keys of d
        sum (loop
              for (r2 c2) being the hash-keys of d
              for d12 = (util:manhattan `(,r1 ,c1) `(,r2 ,c2))
              count (and (= 2 d12)
                         (>= (- (gethash `(,r1 ,c1) d)
                                (gethash `(,r2 ,c2) d)
                                d12)
                             100)))))))

(defun problem-2 (&key (input *input-part-1-test*))
  (multiple-value-bind (arr start end)
      (parse-input input)
    (let ((d (find-distances arr start)))
      (loop
        for (r1 c1) being the hash-keys of d
        sum (loop
              for (r2 c2) being the hash-keys of d
              for d12 = (util:manhattan `(,r1 ,c1) `(,r2 ,c2))
              count (and (< d12 21)
                         (>= (- (gethash `(,r1 ,c1) d)
                                (gethash `(,r2 ,c2) d)
                                d12)
                             100)))))))

(defparameter *input-part-1-test*
  "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  "#############################################################################################################################################
#...#.....###.....#.......................###...#...#...#####...#.....#...#...#...........#...#...#...###...#...###.......###...........#...#
#.#.#.###.###.###.#.#####################.###.#.#.#.#.#.#####.#.#.###.#.#.#.#.#.#########.#.#.#.#.#.#.###.#.#.#.###.#####.###.#########.#.#.#
#.#.#...#.....#...#...#...#.....#.........#...#.#.#...#...#...#.#...#...#.#.#.#.....#.....#.#...#...#.....#...#...#.#.....#...#.........#.#.#
#.#.###.#######.#####.#.#.#.###.#.#########.###.#.#######.#.###.###.#####.#.#.#####.#.#####.#####################.#.#.#####.###.#########.#.#
#.#...#.......#...###...#.#.#...#.#.....###...#.#.#...#...#...#.....#.....#.#.#...#.#.....#.#.....#...........#...#.#.#...#.#...#...###...#.#
#.###.#######.###.#######.#.#.###.#.###.#####.#.#.#.#.#.#####.#######.#####.#.#.#.#.#####.#.#.###.#.#########.#.###.#.#.#.#.#.###.#.###.###.#
#.#...###.....#...#.....#...#...#.#.#...#.....#.#...#.#.#...#.......#...#...#.#.#...#.....#...#...#.#...#...#...#...#.#.#...#...#.#...#.#...#
#.#.#####.#####.###.###.#######.#.#.#.###.#####.#####.#.#.#.#######.###.#.###.#.#####.#########.###.#.#.#.#.#####.###.#.#######.#.###.#.#.###
#.#.#...#.....#...#.#...#.......#...#...#.....#.###...#...#.#...#...#...#.#...#.#.....#...#...#...#...#...#...###.#...#.#.......#...#...#...#
#.#.#.#.#####.###.#.#.###.#############.#####.#.###.#######.#.#.#.###.###.#.###.#.#####.#.#.#.###.###########.###.#.###.#.#########.#######.#
#.#...#.#...#.#...#.#...#.......#.......#.....#...#...#...#.#.#.#.#...###.#.###.#.#...#.#.#.#.#...#...........#...#...#.#.......#...#.......#
#.#####.#.#.#.#.###.###.#######.#.#######.#######.###.#.#.#.#.#.#.#.#####.#.###.#.#.#.#.#.#.#.#.###.###########.#####.#.#######.#.###.#######
#.....#.#.#...#...#...#...###...#.......#.......#...#.#.#...#.#...#.#...#.#...#.#.#.#.#.#.#.#.#...#.....#...###...#...#.#.......#.#...#.....#
#####.#.#.#######.###.###.###.#########.#######.###.#.#.#####.#####.#.#.#.###.#.#.#.#.#.#.#.#.###.#####.#.#.#####.#.###.#.#######.#.###.###.#
#...#.#.#.#.......###...#.#...#...#.....#...#...#...#.#...###.....#...#.#...#.#.#.#.#.#.#.#.#.###.#.....#.#.#...#.#.....#.........#.....#...#
#.#.#.#.#.#.###########.#.#.###.#.#.#####.#.#.###.###.###.#######.#####.###.#.#.#.#.#.#.#.#.#.###.#.#####.#.#.#.#.#######################.###
#.#...#.#.#.#.....#.....#.#...#.#...#...#.#.#...#.#...#...#.....#.....#.#...#...#...#...#.#.#.....#.....#.#...#.#...#.....................###
#.#####.#.#.#.###.#.#####.###.#.#####.#.#.#.###.#.#.###.###.###.#####.#.#.###############.#.###########.#.#####.###.#.#######################
#.....#.#.#...#...#.....#.....#.#.....#.#.#.#...#.#...#.....#...#...#.#.#.#...........#...#...#.........#...#...#...#.......#.........#...###
#####.#.#.#####.#######.#######.#.#####.#.#.#.###.###.#######.###.#.#.#.#.#.#########.#.#####.#.###########.#.###.#########.#.#######.#.#.###
###...#...#.....###...#.......#.#.....#...#.#...#...#.....#...#...#...#...#.....#...#...#...#.#...........#.#.###...#.....#...#.......#.#...#
###.#######.#######.#.#######.#.#####.#####.###.###.#####.#.###.###############.#.#.#####.#.#.###########.#.#.#####.#.###.#####.#######.###.#
#...#.....#...#...#.#.#.......#.#...#.#.....###.#...###...#...#...............#...#.#...#.#.#.#...#.......#.#...###.#...#.......#...###.#...#
#.###.###.###.#.#.#.#.#.#######.#.#.#.#.#######.#.#####.#####.###############.#####.#.#.#.#.#.#.#.#.#######.###.###.###.#########.#.###.#.###
#...#...#...#...#.#.#.#.......#.#.#.#.#.#...#...#...#...#.....#...#...#...#...#.....#.#.#.#...#.#.#.###...#...#...#.#...#...#...#.#.#...#...#
###.###.###.#####.#.#.#######.#.#.#.#.#.#.#.#.#####.#.###.#####.#.#.#.#.#.#.###.#####.#.#.#####.#.#.###.#.###.###.#.#.###.#.#.#.#.#.#.#####.#
###.....###.#.....#.#.#...###.#.#.#.#.#...#.#...#...#...#.#...#.#.#.#...#.#...#.......#.#.#.....#...#...#...#...#.#.#.....#...#.#.#.#.#.....#
###########.#.#####.#.#.#.###.#.#.#.#.#####.###.#.#####.#.#.#.#.#.#.#####.###.#########.#.#.#########.#####.###.#.#.###########.#.#.#.#.#####
#.......#...#.......#.#.#.#...#.#.#.#.#.....#...#.#...#.#.#.#.#.#.#.....#...#...#.......#.#.....#...#.#.....#...#...#...........#.#.#.#.....#
#.#####.#.###########.#.#.#.###.#.#.#.#.#####.###.#.#.#.#.#.#.#.#.#####.###.###.#.#######.#####.#.#.#.#.#####.#######.###########.#.#.#####.#
#.....#...#.......#...#.#.#...#...#...#.#.....#...#.#.#.#.#.#.#.#.#...#.#...#...#.#.....#.#.....#.#.#.#...#...#...#...#...###...#.#.#.#...#.#
#####.#####.#####.#.###.#.###.#########.#.#####.###.#.#.#.#.#.#.#.#.#.#.#.###.###.#.###.#.#.#####.#.#.###.#.###.#.#.###.#.###.#.#.#.#.#.#.#.#
#...#.....#.#.....#.#...#.....#...#.....#.#.....###.#.#.#.#.#.#.#.#.#.#.#.###...#.#...#.#.#.#...#.#.#.#...#...#.#...#...#.#...#.#.#.#.#.#...#
#.#.#####.#.#.#####.#.#########.#.#.#####.#.#######.#.#.#.#.#.#.#.#.#.#.#.#####.#.###.#.#.#.#.#.#.#.#.#.#####.#.#####.###.#.###.#.#.#.#.#####
#.#.......#.#.#...#...#.....#...#.#...#...#.#...#...#.#.#.#.#.#.#.#.#...#.#...#.#.....#.#.#.#.#.#.#.#.#.......#.....#...#.#.#...#.#...#.....#
#.#########.#.#.#.#####.###.#.###.###.#.###.#.#.#.###.#.#.#.#.#.#.#.#####.#.#.#.#######.#.#.#.#.#.#.#.#############.###.#.#.#.###.#########.#
#.#...#...#.#...#...#...###...###...#...#...#.#.#.###.#.#.#.#...#.#.....#.#.#.#.....#...#.#.#.#.#.#.#.#.........#...#...#.#.#...#.#.........#
#.#.#.#.#.#.#######.#.#############.#####.###.#.#.###.#.#.#.#####.#####.#.#.#.#####.#.###.#.#.#.#.#.#.#.#######.#.###.###.#.###.#.#.#########
#.#.#...#...#...#...#.#...###...###.....#.....#.#...#...#.#...#...#.....#...#.#.....#...#.#.#.#.#.#.#.#.......#...###...#.#...#...#.........#
#.#.#########.#.#.###.#.#.###.#.#######.#######.###.#####.###.#.###.#########.#.#######.#.#.#.#.#.#.#.#######.#########.#.###.#############.#
#...#.........#...#...#.#.#...#.........#...#...#...#.....#...#...#.....#...#.#.......#...#.#.#.#.#...#.......#...#...#.#.###.#...#.........#
#####.#############.###.#.#.#############.#.#.###.###.#####.#####.#####.#.#.#.#######.#####.#.#.#.#####.#######.#.#.#.#.#.###.#.#.#.#########
#.....#...#...#...#.#...#...#...#.........#.#...#...#.......#.....#...#...#.#.#.......#.....#.#.#...###.........#...#...#.....#.#...###...###
#.#####.#.#.#.#.#.#.#.#######.#.#.#########.###.###.#########.#####.#.#####.#.#.#######.#####.#.###.###########################.#######.#.###
#.#...#.#...#.#.#.#.#...#...#.#.#.#.....#...###...#...#.......#...#.#.......#.#.#.......#...#.#.#...#.............#...#.....#...#.....#.#...#
#.#.#.#.#####.#.#.#.###.#.#.#.#.#.#.###.#.#######.###.#.#######.#.#.#########.#.#.#######.#.#.#.#.###.###########.#.#.#.###.#.###.###.#.###.#
#.#.#...#...#...#...###...#.#.#...#...#...#...###.#...#.......#.#.#.........#.#.#.#.....#.#...#.#.###...........#...#...###...#...#...#...#.#
#.#.#####.#.###############.#.#######.#####.#.###.#.#########.#.#.#########.#.#.#.#.###.#.#####.#.#############.###############.###.#####.#.#
#.#.#.....#...#...###...###...#.......#...#.#.#...#.###...###.#.#.#...#.....#...#.#.###.#.#.....#.#...........#...............#.#...#...#.#.#
#.#.#.#######.#.#.###.#.#######.#######.#.#.#.#.###.###.#.###.#.#.#.#.#.#########.#.###.#.#.#####.#.#########.###############.#.#.###.#.#.#.#
#...#.......#...#.#...#...#.....#.......#...#.#...#.#...#.....#.#.#.#.#.......#...#.#...#.#...#...#.#.....###.................#.#.###.#...#.#
###########.#####.#.#####.#.#####.###########.###.#.#.#########.#.#.#.#######.#.###.#.###.###.#.###.#.###.#####################.#.###.#####.#
#...#...###.....#...#...#...#.....#...#...#...#...#.#.#...#...#.#.#.#.#.......#.#...#...#.###.#.###...###...............#.......#.....#...#.#
#.#.#.#.#######.#####.#.#####.#####.#.#.#.#.###.###.#.#.#.#.#.#.#.#.#.#.#######.#.#####.#.###.#.#######################.#.#############.#.#.#
#.#...#.........#...#.#.#...#.#.....#...#...###.....#.#.#.#.#.#.#...#.#.......#...#...#.#.#...#.#...#...................#.#...#.......#.#.#.#
#.###############.#.#.#.#.#.#.#.#####################.#.#.#.#.#.#####.#######.#####.#.#.#.#.###.#.#.#.###################.#.#.#.#####.#.#.#.#
#...#...#...#...#.#.#.#...#...#.....###...#...###...#...#.#.#.#...#...#.....#.......#.#...#.....#.#.#...............#...#.#.#...#...#...#...#
###.#.#.#.#.#.#.#.#.#.#############.###.#.#.#.###.#.#####.#.#.###.#.###.###.#########.###########.#.###############.#.#.#.#.#####.#.#########
#...#.#.#.#.#.#.#.#.#...#.........#.....#...#.....#...###...#...#.#...#...#.#.........###...#...#.#.#.......#.......#.#.#.#.#.....#.........#
#.###.#.#.#.#.#.#.#.###.#.#######.###################.#########.#.###.###.#.#.###########.#.#.#.#.#.#.#####.#.#######.#.#.#.#.#############.#
#.#...#.#.#...#...#...#.#.......#.#.....#...#...#...#.......#...#.#...#...#.#.......#...#.#.#.#.#.#.#.....#...###...#.#...#...#.............#
#.#.###.#.###########.#.#######.#.#.###.#.#.#.#.#.#.#######.#.###.#.###.###.#######.#.#.#.#.#.#.#.#.#####.#######.#.#.#########.#############
#.#...#.#.###.......#...#...#...#...#...#.#...#...#...#.....#.....#...#.###.........#.#.#.#...#.#.#.#...#.......#.#...#...#...#.............#
#.###.#.#.###.#####.#####.#.#.#######.###.###########.#.#############.#.#############.#.#.#####.#.#.#.#.#######.#.#####.#.#.#.#############.#
#...#.#.#.#...#...#...###.#.#.......#...#.........#...#.....#.........#.........#.....#...#...#...#...#.......#...#...#.#.#.#...#...........#
###.#.#.#.#.###.#.###.###.#.#######.###.#########.#.#######.#.#################.#.#########.#.###############.#####.#.#.#.#.###.#.###########
###...#...#.....#...#.#...#.......#...#.......#...#.........#...#.......#.......#...........#...............#.#...#.#.#.#...###.#.....#...###
###################.#.#.#########.###.#######.#.###############.#.#####.#.#################################.#.#.#.#.#.#.#######.#####.#.#.###
###...#...#...#...#.#.#.........#.....#.....#.#.......#...#...#.#.#.....#.......###...#.....................#...#...#...###...#.#...#...#...#
###.#.#.#.#.#.#.#.#.#.#########.#######.###.#.#######.#.#.#.#.#.#.#.###########.###.#.#.###################################.#.#.#.#.#######.#
#...#...#...#...#...#...........#.......#...#.#...#...#.#...#.#...#.#.....#.....#...#...#.....#...#.........................#.#.#.#...#.....#
#.###############################.#######.###.#.#.#.###.#####.#####.#.###.#.#####.#######.###.#.#.#.#########################.#.#.###.#.#####
#.....#...#...........#...#.....#.#.......###...#...#...#.....#.....#.#...#.#.....#.....#...#...#...#.................#.....#.#.#.#...#.....#
#####.#.#.#.#########.#.#.#.###.#.#.#################.###.#####.#####.#.###.#.#####.###.###.#########.###############.#.###.#.#.#.#.#######.#
#...#...#...###...###...#.#.###...#...........#.....#...#.#...#.......#...#.#.#...#.###...#.#.........#.........#...#...###...#...#.......#.#
#.#.###########.#.#######.#.#################.#.###.###.#.#.#.###########.#.#.#.#.#.#####.#.#.#########.#######.#.#.#####################.#.#
#.#...#...#.....#.......#...#.........#.......#.#...#...#.#.#.#.........#...#...#...#...#...#...........#.......#.#.....#...###...#.....#.#.#
#.###.#.#.#.###########.#####.#######.#.#######.#.###.###.#.#.#.#######.#############.#.#################.#######.#####.#.#.###.#.#.###.#.#.#
#...#...#.#.........#...#...#.......#.#.......#.#...#...#...#...#...#...#...###.......#.#.................#...#...#.....#.#.#...#...#...#...#
###.#####.#########.#.###.#.#######.#.#######.#.###.###.#########.#.#.###.#.###.#######.#.#################.#.#.###.#####.#.#.#######.#######
###...#...#...#...#.#.....#.#...#...#.........#.#...#...#.........#...#...#...#.......#.#.......#...#.......#...#...#...#.#.#.#.......#...###
#####.#.###.#.#.#.#.#######.#.#.#.#############.#.###.###.#############.#####.#######.#.#######.#.#.#.###########.###.#.#.#.#.#.#######.#.###
#...#.#.###.#.#.#.#.......#...#.#.....#...#...#.#.....#...###.......###.....#.#.....#.#.#...#...#.#.#.#...........#...#...#...#.###.....#...#
#.#.#.#.###.#.#.#.#######.#####.#####.#.#.#.#.#.#######.#####.#####.#######.#.#.###.#.#.#.#.#.###.#.#.#.###########.###########.###.#######.#
#.#...#.....#...#.....#...#...#.......#.#...#.#.#.......#...#.#.....#...#...#.#...#.#.#.#.#.#.....#...#...###...###.#.....#...#...#.#.......#
#.###################.#.###.#.#########.#####.#.#.#######.#.#.#.#####.#.#.###.###.#.#.#.#.#.#############.###.#.###.#.###.#.#.###.#.#.#######
#...#.............#...#.....#...........#...#.#.#...#...#.#...#...###.#.#...#.###E#.#.#.#.#.#...#.......#.....#.....#...#.#.#...#...#.#...###
###.#.###########.#.#####################.#.#.#.###.#.#.#.#######.###.#.###.#.#####.#.#.#.#.#.#.#.#####.###############.#.#.###.#####.#.#.###
###...#...........#.....#...............#.#...#...#...#...#.....#...#.#.###.#.#####.#.#.#.#...#...#...#.........#...#...#.#...#.....#...#...#
#######.###############.#.#############.#.#######.#########.###.###.#.#.###.#.#####.#.#.#.#########.#.#########.#.#.#.###.###.#####.#######.#
#.....#...............#.#.............#.#...#.....#...#.....###.....#.#...#.#.#####.#.#.#...#.......#.........#.#.#...###.#...#...#.......#.#
#.###.###############.#.#############.#.###.#.#####.#.#.#############.###.#.#.#####.#.#.###.#.###############.#.#.#######.#.###.#.#######.#.#
#...#...........#...#.#...............#.....#.#...#.#.#...#...#.....#...#.#.#.#####.#.#...#...#...#...........#...#.....#...#...#.......#...#
###.###########.#.#.#.#######################.#.#.#.#.###.#.#.#.###.###.#.#.#.#####.#.###.#####.#.#.###############.###.#####.#########.#####
###...#...#...#...#...#.........#...#.....###...#...#.#...#.#.#.#...#...#.#.#.#####.#.#...#.....#.#.#.......###...#.#...###...#...#...#.....#
#####.#.#.#.#.#########.#######.#.#.#.###.###########.#.###.#.#.#.###.###.#.#.#####.#.#.###.#####.#.#.#####.###.#.#.#.#####.###.#.#.#.#####.#
#...#...#...#.#...#...#.#.......#.#...#...#...#...#...#...#.#.#.#...#...#...#...###...#...#...#...#...###...#...#...#.......#...#...#.......#
#.#.#########.#.#.#.#.#.#.#######.#####.###.#.#.#.#.#####.#.#.#.###.###.#######.#########.###.#.#########.###.###############.###############
#.#.#...#...#...#...#...#...#.....#...#.#...#.#.#.#.#.....#.#.#.#...#...#.......###.......###.#.###...#...#...#...#...#.....#.......#...#...#
#.#.#.#.#.#.###############.#.#####.#.#.#.###.#.#.#.#.#####.#.#.#.###.###.#########.#########.#.###.#.#.###.###.#.#.#.#.###.#######.#.#.#.#.#
#.#.#.#...#...#...........#...#.....#...#...#...#.#.#.#...#.#...#.#...#...#########...#...#...#...#.#.#.....#...#...#...###.#.......#.#...#.#
#.#.#.#######.#.#########.#####.###########.#####.#.#.#.#.#.#####.#.###.#############.#.#.#.#####.#.#.#######.#############.#.#######.#####.#
#.#...#.....#...#...#...#.....#.....###...#...#...#.#.#.#.#.....#.#.#...###########...#.#.#...#...#.#.###.....#...........#.#.........#.....#
#.#####.###.#####.#.#.#.#####.#####.###.#.###.#.###.#.#.#.#####.#.#.#.#############.###.#.###.#.###.#.###.#####.#########.#.###########.#####
#.....#.###.#...#.#...#.....#.......#...#.#...#...#.#.#.#...#...#.#.#...###########.#...#.#...#...#.#.#...#...#.#.........#...#.....#...#...#
#####.#.###.#.#.#.#########.#########.###.#.#####.#.#.#.###.#.###.#.###.###########.#.###.#.#####.#.#.#.###.#.#.#.###########.#.###.#.###.#.#
#.....#.#...#.#.#.#.........#...#...#...#.#.#.....#.#.#.#...#...#.#.#...#S#########.#.#...#...###.#.#.#.....#...#...........#...#...#.....#.#
#.#####.#.###.#.#.#.#########.#.#.#.###.#.#.#.#####.#.#.#.#####.#.#.#.###.#########.#.#.#####.###.#.#.#####################.#####.#########.#
#.#...#.#.#...#...#.......#...#.#.#.#...#...#...#...#.#.#.#...#.#.#.#...#.....#####.#.#.#...#...#.#.#...#.......#.........#.....#.....#...#.#
#.#.#.#.#.#.#############.#.###.#.#.#.#########.#.###.#.#.#.#.#.#.#.###.#####.#####.#.#.#.#.###.#.#.###.#.#####.#.#######.#####.#####.#.#.#.#
#.#.#.#.#...#.............#.###.#.#.#.....#...#...###.#.#.#.#.#.#.#.#...#.....#####...#.#.#...#.#.#.#...#...#...#.......#.#.....#.....#.#.#.#
#.#.#.#.#####.#############.###.#.#.#####.#.#.#######.#.#.#.#.#.#.#.#.###.#############.#.###.#.#.#.#.#####.#.#########.#.#.#####.#####.#.#.#
#.#.#.#...#...#.....#.....#...#.#.#.#...#...#.......#...#...#...#...#...#.......#.......#...#.#.#.#.#...#...#.#...#...#.#.#.....#.....#.#.#.#
#.#.#.###.#.###.###.#.###.###.#.#.#.#.#.###########.###################.#######.#.#########.#.#.#.#.###.#.###.#.#.#.#.#.#.#####.#####.#.#.#.#
#.#.#.....#.....###...###...#.#.#.#.#.#.#.....#...#.#...................#...#...#.....#.....#...#.#.#...#.###...#.#.#.#.#.......###...#.#.#.#
#.#.#######################.#.#.#.#.#.#.#.###.#.#.#.#.###################.#.#.#######.#.#########.#.#.###.#######.#.#.#.###########.###.#.#.#
#.#.....#.................#.#.#.#.#.#.#.#...#.#.#...#.#...#...#...#...#...#.#.#...#...#.......#...#.#...#.......#.#.#.#.........#...#...#.#.#
#.#####.#.###############.#.#.#.#.#.#.#.###.#.#.#####.#.#.#.#.#.#.#.#.#.###.#.#.#.#.#########.#.###.###.#######.#.#.#.#########.#.###.###.#.#
#...#...#...........#...#...#.#.#.#.#.#.#...#.#...#...#.#...#.#.#.#.#.#.#...#.#.#.#...#...#...#...#...#.....#...#...#...........#.#...#...#.#
###.#.#############.#.#.#####.#.#.#.#.#.#.###.###.#.###.#####.#.#.#.#.#.#.###.#.#.###.#.#.#.#####.###.#####.#.###################.#.###.###.#
###...#...........#.#.#.....#.#.#.#.#.#.#.###.#...#.....#...#.#.#.#.#.#.#...#...#.#...#.#...###...#...#.....#.......#...#.......#.#...#.....#
#######.#########.#.#.#####.#.#.#.#.#.#.#.###.#.#########.#.#.#.#.#.#.#.###.#####.#.###.#######.###.###.###########.#.#.#.#####.#.###.#######
###.....#.......#.#.#.#.....#.#...#.#.#.#.#...#.......#...#...#.#.#.#.#.#...#.....#...#.....#...#...###...#...#.....#.#.#.#.....#...#.......#
###.#####.#####.#.#.#.#.#####.#####.#.#.#.#.#########.#.#######.#.#.#.#.#.###.#######.#####.#.###.#######.#.#.#.#####.#.#.#.#######.#######.#
#...#.....#.....#...#.#.....#...###...#.#.#.#.....#...#.#...#...#.#.#.#.#...#.......#.#...#.#.#...###.....#.#.#.#.....#...#.......#.#.......#
#.###.#####.#########.#####.###.#######.#.#.#.###.#.###.#.#.#.###.#.#.#.###.#######.#.#.#.#.#.#.#####.#####.#.#.#.###############.#.#.#######
#...#.#.....#...#...#.#...#...#...#.....#.#.#...#...###...#.#...#.#.#.#...#...#.....#...#.#.#.#...#...#...#.#.#...#...#...#.......#.#.#...###
###.#.#.#####.#.#.#.#.#.#.###.###.#.#####.#.###.###########.###.#.#.#.###.###.#.#########.#.#.###.#.###.#.#.#.#####.#.#.#.#.#######.#.#.#.###
#...#.#...#...#...#.#.#.#.....#...#...#...#...#.....#...#...#...#...#.#...#...#.......###...#.#...#...#.#...#.#...#.#.#.#.#.......#...#.#...#
#.###.###.#.#######.#.#.#######.#####.#.#####.#####.#.#.#.###.#######.#.###.#########.#######.#.#####.#.#####.#.#.#.#.#.#.#######.#####.###.#
#.#...#...#.#.......#.#.......#...#...#.#.....#...#.#.#...#...#...#...#...#...#.....#.....###...#...#...#...#.#.#...#...#.#...#...###...#...#
#.#.###.###.#.#######.#######.###.#.###.#.#####.#.#.#.#####.###.#.#.#####.###.#.###.#####.#######.#.#####.#.#.#.#########.#.#.#.#####.###.###
#.#.###...#.#.........###.....#...#...#.#.#...#.#.#.#.#...#.....#.#...#...###...#...#...#...#.....#.#.....#...#...#...###.#.#...#...#...#...#
#.#.#####.#.#############.#####.#####.#.#.#.#.#.#.#.#.#.#.#######.###.#.#########.###.#.###.#.#####.#.###########.#.#.###.#.#####.#.###.###.#
#.#.#.....#...#...........#...#.#.....#.#...#.#.#.#.#.#.#...#...#...#...#...#.....#...#...#.#.....#...#.....#...#...#...#.#...###.#.....#...#
#.#.#.#######.#.###########.#.#.#.#####.#####.#.#.#.#.#.###.#.#.###.#####.#.#.#####.#####.#.#####.#####.###.#.#.#######.#.###.###.#######.###
#.#.#.#...#...#...#...#...#.#.#.#.#...#...#...#.#.#.#.#.#...#.#.#...#.....#...#...#.....#.#.....#...#...#...#.#...#...#.#...#.....#.....#...#
#.#.#.#.#.#.#####.#.#.#.#.#.#.#.#.#.#.###.#.###.#.#.#.#.#.###.#.#.###.#########.#.#####.#.#####.###.#.###.###.###.#.#.#.###.#######.###.###.#
#...#...#...#####...#...#...#...#...#.....#.....#...#...#.....#...###...........#.......#.......###...###.....###...#...###.........###.....#
#############################################################################################################################################")
