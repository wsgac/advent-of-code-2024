(in-package #:advent-of-code-2024.day14)

(defun parse-input (input)
  (mapcar (lambda (line)
            (let ((ints (util:extract-integers line)))
              (list (subseq ints 0 2)
                    (subseq ints 2))))
          (util:split-lines input)))

(defun robot-step (dimensions position velocity)
  (list (mod (+ (first position) (first velocity)) (first dimensions))
        (mod (+ (second position) (second velocity)) (second dimensions))))

(defun robot-n-steps (dimensions position velocity steps)
  (loop
    repeat (1+ steps)
    for pos = position then (robot-step dimensions pos velocity)
    finally (return pos)))

(defun safety-factor (positions dimensions)
  (loop
    with (cols rows) = dimensions
    for (x y) in positions
    count (and (< x (truncate cols 2))
               (< y (truncate rows 2)))
      into q1
    count (and (> x (truncate cols 2))
               (< y (truncate rows 2)))
      into q2
    count (and (< x (truncate cols 2))
               (> y (truncate rows 2)))
      into q3
    count (and (> x (truncate cols 2))
               (> y (truncate rows 2)))
      into q4
    finally (return (* q1 q2 q3 q4))))

(defun problem-1 (&key (input *input-part-1-test*)
                    (dimensions '(11 7)))
  (loop
    for (p v) in (parse-input input)
    collect (robot-n-steps dimensions p v 100) into end-positions
    finally (return (safety-factor end-positions dimensions))))

(defun count-central (positions dimensions)
  "Calculate the percentage of points from `positions` occupying central
positions, i.e. middle 1/4 of the area described by `dimensions`."
  (loop
    with (cols rows) = dimensions
    for ((x y) nil) in positions
    count (and (< (truncate cols 4) x (* 3 (truncate cols 4)))
               (< (truncate rows 4) y (* 3 (truncate rows 4))))
      into central
    finally (return (/ central (length positions)))))

(defun picture-positions (positions dimensions)
  (let ((arr (make-array (reverse dimensions) :initial-element #\space)))
    (loop
      for ((col row) nil) in positions
      do (setf (aref arr row col) #\*))
    (util:2d-array->png arr)))

(defun problem-2 (&key (input *input-part-1-test*)
                    (dimensions '(11 7)))
  "Starting from positions and velocities specified in `input` continue
stepping until most (> 1/2) robots arrange themselves into a
picture. This picture will probably be concentrated around the center,
so just try counting the percentage of robots in the middle 1/4 of our
area."
  (let ((init-robots (parse-input input)))
    (loop
      for i from 0
      for robots = init-robots then (evolve robots 1 dimensions)
      for central = (count-central robots dimensions)
      when (> central 1/2)
        do (return (values i (picture-positions robots dimensions))))))

;; Live session

(defun parse-input (input)
  (mapcar
   (lambda (row)
     (destructuring-bind (a b c d)
         (util:extract-integers row)
       ;; (list (list a b) (list c d))
       `((,a ,b) (,c ,d))
       ))
   (util:split-lines input)))

(defun bound-vec+ (v1 v2 dimensions)
  (mapcar (lambda (c1 c2 d)
            (mod (+ c1 c2) d))
          v1 v2 dimensions))

(defun vec-scale (v scale-factor)
  (mapcar (lambda (c) (* c scale-factor)) v))


;; V = d/t
;; V = dx/dt
(defun evolve (robots seconds grid-dimensions)
  (loop
    for (position velocity) in robots
    for final-position = (bound-vec+ position
                                     (vec-scale velocity
                                                seconds)
                                     grid-dimensions)
    collect (list final-position velocity)))

(defun safety-factor (robots dimensions)
  (loop
    with (cols rows) = dimensions
    with mid-col = (truncate cols 2)
    with mid-row = (truncate rows 2)
    for ((x y)) in robots
    count (and (< x mid-col)
               (< y mid-row))
      into q1
    count (and (> x mid-col)
               (< y mid-row))
      into q2
    count (and (< x mid-col)
               (> y mid-row))
      into q3
    count (and (> x mid-col)
               (> y mid-row))
      into q4
    finally (return (* q1 q2 q3 q4))))

(defun problem-1 (input dimensions)
  (let ((final-state (evolve (parse-input input)
                             100 dimensions)))
    (safety-factor final-state dimensions)))

(defparameter *input-part-1-test*
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  "p=58,57 v=-51,-38
p=26,92 v=-82,30
p=51,30 v=21,-47
p=29,38 v=37,60
p=29,23 v=59,-19
p=7,38 v=-61,12
p=83,51 v=58,-56
p=84,94 v=62,61
p=79,93 v=-95,-32
p=59,34 v=88,93
p=29,86 v=25,79
p=24,6 v=6,23
p=25,25 v=69,-47
p=76,102 v=-45,69
p=39,24 v=-79,19
p=16,46 v=45,-62
p=65,41 v=-47,-68
p=63,19 v=28,-88
p=53,61 v=51,12
p=72,37 v=-23,56
p=4,65 v=-35,80
p=96,89 v=65,41
p=76,71 v=83,-41
p=31,41 v=-8,-42
p=53,4 v=-38,-71
p=70,36 v=80,-68
p=14,37 v=-6,-78
p=29,88 v=53,2
p=39,15 v=-78,-96
p=100,63 v=63,84
p=37,23 v=71,93
p=19,99 v=-30,67
p=5,76 v=60,69
p=83,96 v=7,-26
p=84,78 v=32,49
p=59,29 v=70,73
p=56,27 v=27,97
p=17,6 v=16,-30
p=33,77 v=-67,73
p=36,60 v=-29,-95
p=66,7 v=3,-16
p=22,74 v=91,12
p=71,33 v=-98,6
p=44,93 v=30,-1
p=59,23 v=68,-75
p=98,33 v=11,-72
p=97,22 v=87,-74
p=78,81 v=53,24
p=80,54 v=-57,-36
p=63,25 v=54,16
p=5,85 v=-38,-71
p=83,37 v=84,82
p=93,15 v=-40,54
p=34,89 v=-76,98
p=54,19 v=4,-18
p=33,52 v=-34,72
p=90,74 v=9,51
p=21,12 v=44,83
p=42,43 v=72,41
p=16,85 v=-36,88
p=47,4 v=-52,-30
p=0,69 v=88,81
p=57,33 v=51,64
p=56,26 v=-71,94
p=63,34 v=-71,-78
p=74,100 v=-21,-61
p=69,88 v=58,77
p=46,60 v=-69,-30
p=62,44 v=26,-70
p=38,84 v=98,-75
p=93,26 v=-62,-4
p=71,82 v=42,-21
p=16,45 v=93,74
p=40,31 v=21,-76
p=12,42 v=-60,-68
p=73,35 v=-7,-14
p=19,42 v=-9,-72
p=48,79 v=-27,63
p=73,57 v=30,14
p=96,2 v=83,54
p=40,10 v=46,81
p=88,8 v=-92,-20
p=31,63 v=31,-51
p=29,9 v=-66,-2
p=8,53 v=12,39
p=33,21 v=19,-84
p=46,33 v=-30,6
p=20,35 v=-63,84
p=72,18 v=-20,23
p=82,89 v=7,-75
p=23,3 v=-87,34
p=77,91 v=-53,-80
p=36,4 v=-5,44
p=22,1 v=-55,-84
p=7,6 v=-49,-85
p=38,74 v=-5,53
p=19,87 v=-88,67
p=84,47 v=73,-15
p=36,67 v=22,-54
p=78,10 v=55,-24
p=55,51 v=-26,2
p=0,27 v=63,-76
p=85,11 v=-17,50
p=52,16 v=-47,42
p=0,14 v=47,69
p=82,8 v=7,87
p=13,92 v=86,-56
p=11,11 v=-70,87
p=89,74 v=80,-22
p=23,60 v=-11,37
p=0,67 v=-13,24
p=14,33 v=37,29
p=68,94 v=85,4
p=64,23 v=53,81
p=96,35 v=84,76
p=19,29 v=-45,56
p=72,19 v=-71,-90
p=9,36 v=90,-33
p=59,52 v=-20,80
p=50,100 v=76,85
p=78,57 v=54,-81
p=27,84 v=73,-48
p=81,65 v=-42,-7
p=55,76 v=15,-91
p=86,87 v=-92,-7
p=57,76 v=30,-3
p=28,15 v=81,79
p=58,96 v=3,36
p=52,69 v=76,-60
p=61,88 v=79,30
p=15,70 v=91,22
p=80,5 v=3,52
p=85,22 v=83,60
p=49,8 v=-28,-59
p=100,59 v=66,39
p=100,100 v=62,36
p=74,75 v=-72,59
p=20,34 v=93,-74
p=30,63 v=44,55
p=71,101 v=29,-92
p=57,59 v=77,-56
p=84,29 v=25,53
p=41,81 v=-33,-79
p=45,31 v=-2,39
p=11,89 v=83,-19
p=7,67 v=-52,-59
p=29,44 v=-6,78
p=48,7 v=-3,23
p=36,17 v=92,25
p=1,29 v=-88,-41
p=47,55 v=75,-56
p=10,64 v=-35,-91
p=5,82 v=-87,26
p=19,22 v=13,-82
p=43,55 v=-77,6
p=71,59 v=-95,78
p=49,45 v=-25,65
p=31,72 v=-82,-83
p=71,59 v=4,-25
p=88,2 v=54,-59
p=64,67 v=-28,92
p=68,75 v=-43,-11
p=34,66 v=-1,-87
p=28,63 v=50,-62
p=9,93 v=57,-67
p=58,57 v=-22,41
p=21,59 v=67,8
p=28,24 v=-53,-14
p=76,88 v=-19,65
p=96,97 v=35,-20
p=28,93 v=42,-73
p=39,70 v=-56,84
p=77,19 v=-66,52
p=68,31 v=1,52
p=99,23 v=-41,81
p=84,23 v=-27,60
p=26,59 v=59,67
p=96,83 v=-63,-98
p=67,65 v=-65,80
p=92,78 v=10,59
p=75,14 v=13,38
p=15,64 v=65,80
p=46,99 v=-53,63
p=12,15 v=-10,-80
p=55,11 v=-23,-43
p=33,99 v=-86,41
p=98,55 v=-77,-18
p=49,48 v=-45,21
p=100,9 v=-93,-12
p=60,87 v=-99,-9
p=55,101 v=-99,71
p=13,18 v=39,-88
p=33,88 v=-56,20
p=31,15 v=-27,-6
p=49,13 v=-82,-80
p=14,79 v=85,-26
p=22,74 v=-4,-44
p=97,8 v=83,38
p=51,4 v=-85,-10
p=34,46 v=56,24
p=14,64 v=-9,55
p=50,102 v=-55,36
p=34,77 v=72,-83
p=87,57 v=8,35
p=52,65 v=-49,-21
p=15,47 v=-36,-6
p=22,26 v=-83,91
p=27,38 v=65,2
p=78,29 v=-44,72
p=14,58 v=-77,-92
p=9,75 v=39,90
p=17,95 v=-86,36
p=88,53 v=-96,73
p=38,92 v=-8,20
p=48,96 v=26,61
p=80,82 v=84,-17
p=35,99 v=23,-53
p=93,58 v=90,-54
p=49,25 v=48,-32
p=65,30 v=73,-45
p=39,59 v=23,47
p=6,67 v=52,-48
p=59,26 v=-25,-78
p=23,3 v=-33,83
p=12,49 v=-12,-68
p=42,66 v=-79,55
p=52,94 v=76,1
p=28,33 v=21,-10
p=10,96 v=36,9
p=36,66 v=-51,47
p=40,32 v=71,62
p=94,71 v=36,-3
p=50,9 v=46,44
p=68,48 v=58,-54
p=12,87 v=53,40
p=52,92 v=-23,-92
p=13,68 v=90,82
p=18,58 v=66,12
p=72,89 v=5,1
p=37,77 v=-4,90
p=17,89 v=-55,10
p=47,30 v=-60,49
p=6,79 v=54,-15
p=89,46 v=-41,-25
p=95,65 v=-64,82
p=57,29 v=3,-80
p=79,51 v=76,-8
p=59,97 v=-48,-67
p=68,12 v=53,-20
p=1,27 v=-88,31
p=61,84 v=2,34
p=52,76 v=-51,-79
p=28,90 v=22,57
p=38,78 v=-52,55
p=81,52 v=78,-74
p=8,79 v=88,-13
p=52,94 v=-51,-38
p=39,12 v=-83,7
p=46,97 v=-20,7
p=28,23 v=94,-84
p=30,49 v=-31,4
p=50,92 v=99,54
p=1,32 v=76,55
p=44,12 v=-57,-95
p=67,60 v=78,-54
p=46,78 v=-99,84
p=48,74 v=34,56
p=19,39 v=26,-19
p=21,8 v=-84,17
p=80,74 v=95,-3
p=21,6 v=69,-28
p=35,98 v=-84,-61
p=87,62 v=-91,78
p=66,71 v=73,-34
p=42,24 v=44,95
p=89,22 v=-72,26
p=54,67 v=-97,-87
p=78,17 v=-77,87
p=92,63 v=91,-1
p=59,56 v=-36,92
p=11,50 v=-84,99
p=28,83 v=71,-42
p=80,48 v=-44,2
p=41,3 v=-5,89
p=84,71 v=-68,14
p=87,79 v=-41,65
p=7,101 v=68,-96
p=18,7 v=-84,77
p=51,70 v=74,-52
p=41,20 v=-68,-17
p=32,19 v=49,15
p=1,68 v=87,84
p=86,60 v=1,45
p=98,46 v=88,39
p=92,42 v=-95,78
p=93,86 v=-93,47
p=37,92 v=73,-19
p=100,42 v=-85,-14
p=33,84 v=-30,-69
p=32,49 v=1,38
p=51,12 v=-51,-88
p=96,49 v=44,77
p=49,54 v=-4,33
p=71,90 v=-48,-3
p=65,49 v=-43,-3
p=20,70 v=-85,86
p=18,95 v=40,-25
p=84,16 v=4,97
p=94,43 v=-94,60
p=54,22 v=24,-14
p=63,4 v=70,-77
p=88,68 v=42,48
p=60,1 v=-24,-94
p=33,83 v=97,-40
p=71,47 v=-17,-60
p=42,65 v=23,66
p=13,93 v=13,-57
p=14,91 v=67,-3
p=47,34 v=52,41
p=66,63 v=-10,57
p=64,19 v=27,-80
p=23,87 v=-58,67
p=50,49 v=24,-29
p=48,38 v=-76,-41
p=30,87 v=98,55
p=79,60 v=32,78
p=66,16 v=-98,-37
p=80,8 v=7,85
p=64,101 v=89,7
p=86,93 v=9,-44
p=14,81 v=92,57
p=32,39 v=-81,2
p=65,76 v=56,-21
p=5,102 v=-86,3
p=7,87 v=39,73
p=91,12 v=-42,-92
p=54,41 v=24,66
p=81,43 v=-45,70
p=29,72 v=20,-50
p=8,23 v=-58,93
p=89,62 v=30,74
p=17,18 v=-82,-4
p=27,92 v=69,7
p=68,10 v=5,48
p=72,19 v=-73,15
p=44,20 v=-51,-53
p=97,27 v=91,-53
p=60,72 v=37,-32
p=24,83 v=60,-84
p=82,64 v=-43,43
p=87,5 v=-42,17
p=62,17 v=52,-53
p=64,41 v=-82,98
p=35,44 v=17,-95
p=46,50 v=-76,-95
p=90,58 v=58,-33
p=76,22 v=-44,-86
p=3,90 v=-11,67
p=75,36 v=-69,-4
p=0,73 v=-38,28
p=80,58 v=-5,-47
p=49,41 v=73,-14
p=39,42 v=-54,70
p=67,45 v=-18,-23
p=43,29 v=-35,-91
p=74,70 v=-7,-21
p=81,20 v=-70,4
p=41,3 v=-4,-24
p=78,38 v=-43,-6
p=55,91 v=77,79
p=89,25 v=-67,56
p=36,11 v=-31,81
p=81,68 v=6,-54
p=19,64 v=13,-83
p=43,28 v=-47,53
p=27,38 v=94,37
p=4,23 v=88,-82
p=59,98 v=54,71
p=23,37 v=43,64
p=21,20 v=-80,-43
p=74,15 v=-49,50
p=26,99 v=78,-78
p=55,96 v=2,69
p=92,21 v=-95,-88
p=50,62 v=7,-61
p=82,38 v=89,-76
p=63,12 v=2,56
p=95,55 v=87,41
p=59,48 v=75,-68
p=82,92 v=58,-36
p=40,76 v=-55,-34
p=36,48 v=50,-19
p=5,71 v=32,76
p=55,39 v=50,-70
p=0,85 v=41,-79
p=52,48 v=-26,16
p=65,86 v=-47,-3
p=68,89 v=30,-88
p=7,4 v=10,15
p=57,71 v=80,10
p=75,0 v=32,9
p=18,95 v=-59,26
p=26,42 v=44,-29
p=52,17 v=-28,48
p=63,3 v=79,46
p=61,68 v=-75,-48
p=88,96 v=5,79
p=26,89 v=22,22
p=64,38 v=-24,62
p=44,24 v=24,-78
p=26,39 v=-66,-71
p=15,40 v=40,-33
p=85,35 v=-93,-70
p=8,19 v=-65,-72
p=40,44 v=-82,-47
p=63,80 v=28,20
p=45,75 v=-28,-79
p=24,72 v=-58,-71
p=96,99 v=-44,-65
p=91,84 v=54,-27
p=55,47 v=80,-13
p=50,35 v=87,-57
p=76,98 v=-99,-98
p=72,67 v=80,57
p=69,38 v=68,-55
p=50,30 v=46,64
p=2,67 v=92,-44
p=22,7 v=-78,45
p=74,12 v=52,13
p=11,74 v=1,76
p=55,55 v=-67,-55
p=38,63 v=42,62
p=90,85 v=-65,94
p=20,36 v=-8,68
p=50,57 v=-98,-83
p=32,69 v=-6,30
p=30,35 v=97,-74
p=4,81 v=95,-88
p=70,86 v=-47,94
p=99,72 v=33,-89
p=48,59 v=74,-93
p=95,98 v=-66,36
p=6,64 v=96,-14
p=88,19 v=85,-80
p=16,41 v=-35,-39
p=70,71 v=81,20
p=85,58 v=58,8
p=59,40 v=-78,-25
p=51,39 v=-25,35
p=99,52 v=42,38
p=91,18 v=74,-14
p=65,86 v=28,67
p=92,50 v=64,6
p=86,97 v=57,-32
p=87,17 v=59,-51
p=36,59 v=74,68
p=19,11 v=-88,7
p=38,88 v=72,90
p=38,45 v=-4,-62
p=59,78 v=-59,28
p=92,13 v=44,80
p=35,53 v=-28,-2
p=87,22 v=-68,-24
p=20,64 v=-20,-33
p=83,76 v=-68,55
p=89,61 v=60,-37
p=2,35 v=37,31
p=12,62 v=-14,-54
p=7,97 v=-10,-48
p=83,99 v=84,-26
p=74,0 v=-20,-26
p=34,10 v=-80,-84
p=14,1 v=60,-30
p=12,94 v=15,-38
p=36,46 v=22,-41
p=35,69 v=42,-43
p=16,96 v=41,1
p=8,24 v=-10,21
p=60,89 v=3,89
p=70,13 v=49,-28
p=9,49 v=-10,39
p=100,31 v=-64,93
p=57,44 v=-72,-30
p=29,25 v=96,-8
p=19,75 v=-34,-79
p=26,18 v=94,19
p=6,9 v=-61,-10
p=24,23 v=-33,-47
p=23,56 v=-71,25
p=3,23 v=12,60
p=73,14 v=32,81
p=15,41 v=-9,-37
p=36,63 v=-6,82
p=71,56 v=-21,-25
p=60,90 v=78,3
p=52,37 v=-4,-47
p=52,61 v=54,20
p=66,84 v=31,-35
p=35,8 v=46,-26
p=90,28 v=-17,-10")
