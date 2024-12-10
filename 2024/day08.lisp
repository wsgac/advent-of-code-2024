(in-package #:advent-of-code-2024.day08)


(defun parse-input (input)
  (util:parse-string-into-array input))

(defun classify-antennas (arr)
  (let ((h (make-hash-table)))
    (loop
      for row from 0 below (array-dimension arr 0)
      do (loop
           for col from 0 below (array-dimension arr 1)
           for c = (aref arr row col)
           if (alphanumericp c)
             do (push (cons row col) (gethash c h))))
    h))

(defun find-antinodes (locations arr &key (antinode-pairing #'pair-antinodes))
  (loop
    for (l1 . rest) on locations
    while rest
    append (loop
             for l2 in rest
             append (funcall antinode-pairing l1 l2 arr))))

(defun pair-antinodes (l1 l2 arr)
  (let* ((dr (- (car l2) (car l1)))
         (dc (- (cdr l2) (cdr l1))))
    (remove-if-not (lambda (p)
                     (ls-user:array-in-bounds-p arr (car p) (cdr p)))
                   (list (cons (- (car l1) dr) (- (cdr l1) dc))
                         (cons (+ (car l2) dr) (+ (cdr l2) dc))))))

(defun pair-antinodes-2 (l1i l2i arr)
  (let* ((dr (- (car l2i) (car l1i)))
         (dc (- (cdr l2i) (cdr l1i))))
    (append
     (loop
       for l1 = l1i then (cons (- (car l1) dr) (- (cdr l1) dc))
       while (ls-user:array-in-bounds-p arr (car l1) (cdr l1))
       collect l1)
     (loop
       for l2 = l2i then (cons (+ (car l2) dr) (+ (cdr l2) dc))
       while (ls-user:array-in-bounds-p arr (car l2) (cdr l2))
       collect l2))))

(defun problem-1 (&key (input *input-part-1-test*))
  (let* ((grid (parse-input input))
         (antennas (classify-antennas grid)))
    (loop
      for locations being the hash-values of antennas
      append (find-antinodes locations grid) into antinodes
      finally (return (length (remove-duplicates antinodes :test #'equal))))))

(defun problem-2 (&key (input *input-part-1-test*))
  (let* ((grid (parse-input input))
         (antennas (classify-antennas grid)))
    (loop
      for locations being the hash-values of antennas
      append (find-antinodes locations grid :antinode-pairing #'pair-antinodes-2) into antinodes
      finally (return (length (remove-duplicates antinodes :test #'equal))))))

(defparameter *input-part-1-test*
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
 ".E..........m..0N.........f.......................
........N........P0...............................
.......j..................................F.......
........1j............P........................C..
...........................3..K......f..........E.
...........V...y...0.....................F........
1.......j.....P....y.N.......................F....
....................m...................C.........
..L......P....p..................w.m..............
............E......p..AU........8......f..........
..............C...............w....d..............
j1...............E..........3.........f........w..
.................p...A..........3.................
.................3..p........KU...w..r..F.........
7.........y........8.......................r......
........y..u......K...............................
...1..................8....C...K..................
...........h.......................6..............
......................U.........A.r..t........6...
...........5.........8..c.........................
.................U................t...............
.....L...O...................t.............d......
.........7........................................
......L..H...c.....9....t.................6.......
...........................c.M..................4.
.....R..7...O.....................................
.......................9......................d...
..................................................
.........L..9...R..........................6c.....
..M.....T.5.................................d.....
.......5OR...................T....................
.......D......o.........v...................r.....
...u....o.........5...............................
.......WR.....Y...........................e...4...
T............O......M..................4..a.......
.Y...................M............................
........W..D...............oh............e........
.......7......Do...................A...e.......4..
.W...Y..D........................h...v..........e.
..........V.....9.l.......h.......a.........n..v..
.......................H.....a2...................
..................................................
...V............Y....J..H2................vn......
..............................H2.................n
................V..........l...........k..........
.T..u........................J...ak...............
..................J.....l.........................
.................l................................
......u.........................................n.
......................J..k............2...........")