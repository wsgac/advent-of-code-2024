(in-package #:advent-of-code-2024.day13)

(defun parse-input (input)
  (mapcar #'parse-machine (re:split "\\n\\n" input)))

(defun parse-machine (spec)
  (destructuring-bind (ax ay bx by px py)
      (mapcar #'parse-integer (re:all-matches-as-strings "[0-9]+" spec))
    `(:prize (,px ,py) :a (,ax ,ay) :b (,bx ,by))))

(defun minimum-cost (machine)
  (tr:match machine
    ((list :prize (list px py)
           :a (list ax ay)
           :b (list bx by))
     (loop
       for a from 0 to 100
       for inner = (loop
                     for b from 0 to 100
                     when (and (or (plusp a) (plusp b))
                               (= px (+ (* a ax) (* b bx)))
                               (= py (+ (* a ay) (* b by))))
                       minimize (+ (* 3 a) b))
       when (plusp inner)
         minimize inner))))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    for machine in (parse-input input)
    sum (minimum-cost machine)))

(defun minimum-cost-2 (machine)
  (tr:match machine
    ((list :prize (list px py)
           :a (list ax ay)
           :b (list bx by))
     (let* ((px (+ px 10000000000000))
            (py (+ py 10000000000000))
            (a (truncate (- (* px by) (* py bx))
                         (- (* by ax) (* bx ay))))
            (b (truncate (- (* px ay) (* py ax))
                         (- (* ay bx) (* by ax)))))
       (if (and (= px (+ (* a ax) (* b bx)))
                (= py (+ (* a ay) (* b by))))
           (+ (* 3 a) b)
           0)))))

(defun problem-2 (&key (input *input-part-1-test*))
  (loop
    for machine in (parse-input input)
    sum (minimum-cost-2 machine)))


(defparameter *input-part-1-test*
  "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defparameter *input-part-2-test*
  *input-part-1-test*)

(defparameter *input*
  "Button A: X+15, Y+61
Button B: X+66, Y+12
Prize: X=1100, Y=4824

Button A: X+14, Y+41
Button B: X+70, Y+38
Prize: X=12198, Y=17542

Button A: X+54, Y+85
Button B: X+44, Y+14
Prize: X=17802, Y=19059

Button A: X+25, Y+54
Button B: X+44, Y+25
Prize: X=2166, Y=8364

Button A: X+88, Y+40
Button B: X+19, Y+86
Prize: X=1843, Y=1534

Button A: X+23, Y+51
Button B: X+52, Y+19
Prize: X=19209, Y=9123

Button A: X+19, Y+85
Button B: X+89, Y+65
Prize: X=6204, Y=11430

Button A: X+75, Y+23
Button B: X+17, Y+59
Prize: X=3347, Y=1261

Button A: X+12, Y+21
Button B: X+40, Y+14
Prize: X=2424, Y=14746

Button A: X+12, Y+46
Button B: X+59, Y+34
Prize: X=14829, Y=15628

Button A: X+97, Y+11
Button B: X+29, Y+24
Prize: X=2644, Y=1149

Button A: X+53, Y+14
Button B: X+27, Y+59
Prize: X=3285, Y=6949

Button A: X+94, Y+38
Button B: X+18, Y+37
Prize: X=3798, Y=2219

Button A: X+72, Y+42
Button B: X+17, Y+38
Prize: X=17323, Y=16192

Button A: X+39, Y+50
Button B: X+88, Y+29
Prize: X=7639, Y=4932

Button A: X+22, Y+74
Button B: X+89, Y+21
Prize: X=7628, Y=2832

Button A: X+15, Y+71
Button B: X+80, Y+86
Prize: X=8630, Y=13338

Button A: X+90, Y+40
Button B: X+64, Y+95
Prize: X=9688, Y=5770

Button A: X+45, Y+14
Button B: X+40, Y+73
Prize: X=2680, Y=14646

Button A: X+22, Y+65
Button B: X+58, Y+17
Prize: X=2522, Y=11867

Button A: X+69, Y+32
Button B: X+13, Y+32
Prize: X=6567, Y=12896

Button A: X+41, Y+18
Button B: X+52, Y+74
Prize: X=2232, Y=15008

Button A: X+35, Y+73
Button B: X+55, Y+13
Prize: X=8090, Y=5350

Button A: X+15, Y+71
Button B: X+63, Y+19
Prize: X=18467, Y=18835

Button A: X+44, Y+79
Button B: X+30, Y+11
Prize: X=15610, Y=1578

Button A: X+32, Y+11
Button B: X+54, Y+84
Prize: X=13002, Y=19632

Button A: X+14, Y+35
Button B: X+46, Y+21
Prize: X=10866, Y=12793

Button A: X+12, Y+54
Button B: X+45, Y+12
Prize: X=13232, Y=1772

Button A: X+12, Y+49
Button B: X+77, Y+18
Prize: X=6534, Y=13276

Button A: X+25, Y+12
Button B: X+36, Y+59
Prize: X=18630, Y=13910

Button A: X+34, Y+97
Button B: X+71, Y+51
Prize: X=3646, Y=3127

Button A: X+40, Y+93
Button B: X+92, Y+43
Prize: X=5040, Y=3173

Button A: X+85, Y+25
Button B: X+14, Y+73
Prize: X=12155, Y=12980

Button A: X+19, Y+75
Button B: X+74, Y+45
Prize: X=7739, Y=7815

Button A: X+22, Y+96
Button B: X+56, Y+36
Prize: X=1846, Y=1596

Button A: X+62, Y+61
Button B: X+22, Y+95
Prize: X=7840, Y=13802

Button A: X+18, Y+14
Button B: X+21, Y+91
Prize: X=2958, Y=6482

Button A: X+26, Y+45
Button B: X+60, Y+32
Prize: X=13212, Y=19598

Button A: X+98, Y+15
Button B: X+37, Y+76
Prize: X=2787, Y=2185

Button A: X+71, Y+25
Button B: X+11, Y+80
Prize: X=5124, Y=8275

Button A: X+45, Y+58
Button B: X+55, Y+22
Prize: X=8120, Y=6848

Button A: X+91, Y+30
Button B: X+29, Y+49
Prize: X=9624, Y=5618

Button A: X+69, Y+65
Button B: X+65, Y+14
Prize: X=10987, Y=7280

Button A: X+19, Y+40
Button B: X+71, Y+44
Prize: X=8700, Y=19020

Button A: X+25, Y+46
Button B: X+87, Y+46
Prize: X=4438, Y=5428

Button A: X+13, Y+52
Button B: X+69, Y+29
Prize: X=18178, Y=15048

Button A: X+95, Y+61
Button B: X+25, Y+56
Prize: X=10590, Y=10515

Button A: X+32, Y+34
Button B: X+11, Y+57
Prize: X=2184, Y=4858

Button A: X+63, Y+21
Button B: X+25, Y+57
Prize: X=14709, Y=1211

Button A: X+97, Y+24
Button B: X+27, Y+32
Prize: X=2844, Y=2552

Button A: X+29, Y+11
Button B: X+21, Y+28
Prize: X=1639, Y=802

Button A: X+56, Y+39
Button B: X+24, Y+88
Prize: X=5432, Y=7276

Button A: X+84, Y+74
Button B: X+98, Y+11
Prize: X=12054, Y=7229

Button A: X+93, Y+21
Button B: X+39, Y+72
Prize: X=6093, Y=5673

Button A: X+62, Y+24
Button B: X+38, Y+63
Prize: X=2894, Y=1410

Button A: X+58, Y+17
Button B: X+18, Y+65
Prize: X=2478, Y=13671

Button A: X+33, Y+13
Button B: X+36, Y+48
Prize: X=2627, Y=15947

Button A: X+15, Y+28
Button B: X+48, Y+19
Prize: X=7508, Y=865

Button A: X+47, Y+19
Button B: X+46, Y+77
Prize: X=14571, Y=5467

Button A: X+43, Y+22
Button B: X+30, Y+55
Prize: X=4175, Y=11485

Button A: X+34, Y+69
Button B: X+77, Y+25
Prize: X=9005, Y=8430

Button A: X+36, Y+11
Button B: X+15, Y+35
Prize: X=11576, Y=2226

Button A: X+59, Y+53
Button B: X+17, Y+98
Prize: X=1580, Y=3653

Button A: X+54, Y+21
Button B: X+26, Y+45
Prize: X=14030, Y=11255

Button A: X+53, Y+13
Button B: X+21, Y+70
Prize: X=8877, Y=8809

Button A: X+31, Y+49
Button B: X+41, Y+14
Prize: X=15910, Y=14515

Button A: X+17, Y+36
Button B: X+46, Y+20
Prize: X=16270, Y=19440

Button A: X+77, Y+39
Button B: X+41, Y+96
Prize: X=5501, Y=4968

Button A: X+26, Y+62
Button B: X+25, Y+11
Prize: X=16893, Y=13607

Button A: X+34, Y+74
Button B: X+60, Y+21
Prize: X=17768, Y=3947

Button A: X+81, Y+16
Button B: X+51, Y+76
Prize: X=11142, Y=8332

Button A: X+31, Y+51
Button B: X+43, Y+11
Prize: X=11989, Y=8501

Button A: X+50, Y+95
Button B: X+80, Y+30
Prize: X=5600, Y=2710

Button A: X+11, Y+35
Button B: X+50, Y+22
Prize: X=1835, Y=1863

Button A: X+12, Y+70
Button B: X+77, Y+12
Prize: X=9596, Y=3514

Button A: X+66, Y+55
Button B: X+79, Y+11
Prize: X=5413, Y=1166

Button A: X+34, Y+62
Button B: X+45, Y+13
Prize: X=2062, Y=3510

Button A: X+64, Y+19
Button B: X+13, Y+38
Prize: X=14897, Y=9012

Button A: X+56, Y+95
Button B: X+48, Y+20
Prize: X=7800, Y=8625

Button A: X+63, Y+14
Button B: X+21, Y+65
Prize: X=5103, Y=1858

Button A: X+40, Y+15
Button B: X+24, Y+38
Prize: X=11976, Y=16662

Button A: X+76, Y+42
Button B: X+20, Y+53
Prize: X=8052, Y=8267

Button A: X+84, Y+84
Button B: X+12, Y+82
Prize: X=5652, Y=7262

Button A: X+27, Y+15
Button B: X+25, Y+42
Prize: X=8491, Y=10412

Button A: X+16, Y+38
Button B: X+57, Y+15
Prize: X=9186, Y=18280

Button A: X+72, Y+42
Button B: X+14, Y+36
Prize: X=3906, Y=18884

Button A: X+55, Y+55
Button B: X+88, Y+23
Prize: X=4554, Y=4359

Button A: X+33, Y+54
Button B: X+36, Y+16
Prize: X=5975, Y=10754

Button A: X+11, Y+38
Button B: X+67, Y+15
Prize: X=3804, Y=5210

Button A: X+59, Y+41
Button B: X+14, Y+39
Prize: X=9057, Y=5595

Button A: X+25, Y+80
Button B: X+68, Y+18
Prize: X=12616, Y=14376

Button A: X+66, Y+14
Button B: X+17, Y+80
Prize: X=7487, Y=18146

Button A: X+68, Y+22
Button B: X+22, Y+73
Prize: X=17498, Y=11487

Button A: X+58, Y+13
Button B: X+40, Y+84
Prize: X=13378, Y=12369

Button A: X+62, Y+12
Button B: X+18, Y+44
Prize: X=16242, Y=1796

Button A: X+11, Y+64
Button B: X+52, Y+26
Prize: X=5149, Y=3686

Button A: X+81, Y+12
Button B: X+14, Y+66
Prize: X=10267, Y=15080

Button A: X+65, Y+31
Button B: X+53, Y+91
Prize: X=6180, Y=7548

Button A: X+30, Y+18
Button B: X+13, Y+66
Prize: X=799, Y=1236

Button A: X+99, Y+89
Button B: X+18, Y+90
Prize: X=5904, Y=6784

Button A: X+20, Y+51
Button B: X+65, Y+29
Prize: X=16110, Y=844

Button A: X+43, Y+15
Button B: X+12, Y+26
Prize: X=6012, Y=8280

Button A: X+65, Y+18
Button B: X+23, Y+68
Prize: X=7316, Y=360

Button A: X+37, Y+74
Button B: X+96, Y+42
Prize: X=8952, Y=5304

Button A: X+88, Y+54
Button B: X+39, Y+87
Prize: X=9906, Y=10998

Button A: X+12, Y+52
Button B: X+70, Y+19
Prize: X=14752, Y=5230

Button A: X+18, Y+44
Button B: X+78, Y+48
Prize: X=15296, Y=17132

Button A: X+26, Y+62
Button B: X+62, Y+24
Prize: X=2212, Y=2674

Button A: X+63, Y+68
Button B: X+14, Y+68
Prize: X=1582, Y=1972

Button A: X+54, Y+20
Button B: X+38, Y+71
Prize: X=14686, Y=17503

Button A: X+48, Y+20
Button B: X+39, Y+69
Prize: X=605, Y=13099

Button A: X+54, Y+92
Button B: X+56, Y+26
Prize: X=8946, Y=10244

Button A: X+33, Y+91
Button B: X+29, Y+18
Prize: X=2476, Y=2242

Button A: X+18, Y+49
Button B: X+31, Y+15
Prize: X=1887, Y=7779

Button A: X+40, Y+13
Button B: X+69, Y+94
Prize: X=6570, Y=8577

Button A: X+69, Y+38
Button B: X+19, Y+69
Prize: X=8015, Y=9975

Button A: X+25, Y+71
Button B: X+57, Y+13
Prize: X=3968, Y=8134

Button A: X+21, Y+39
Button B: X+34, Y+17
Prize: X=2972, Y=17696

Button A: X+22, Y+18
Button B: X+19, Y+88
Prize: X=1874, Y=3562

Button A: X+51, Y+23
Button B: X+21, Y+59
Prize: X=14513, Y=9567

Button A: X+15, Y+95
Button B: X+86, Y+84
Prize: X=7245, Y=11335

Button A: X+53, Y+13
Button B: X+32, Y+80
Prize: X=12593, Y=19449

Button A: X+37, Y+14
Button B: X+53, Y+77
Prize: X=13293, Y=2468

Button A: X+60, Y+76
Button B: X+92, Y+16
Prize: X=7792, Y=2732

Button A: X+73, Y+92
Button B: X+62, Y+24
Prize: X=6901, Y=7452

Button A: X+42, Y+21
Button B: X+24, Y+54
Prize: X=10922, Y=12071

Button A: X+91, Y+12
Button B: X+39, Y+48
Prize: X=8502, Y=2364

Button A: X+89, Y+19
Button B: X+88, Y+87
Prize: X=7861, Y=5771

Button A: X+12, Y+39
Button B: X+71, Y+13
Prize: X=19402, Y=18762

Button A: X+35, Y+25
Button B: X+19, Y+80
Prize: X=3569, Y=7930

Button A: X+75, Y+38
Button B: X+24, Y+87
Prize: X=3705, Y=8987

Button A: X+19, Y+58
Button B: X+63, Y+11
Prize: X=15107, Y=7099

Button A: X+24, Y+56
Button B: X+67, Y+19
Prize: X=6778, Y=2906

Button A: X+47, Y+19
Button B: X+38, Y+59
Prize: X=17067, Y=17501

Button A: X+19, Y+73
Button B: X+57, Y+12
Prize: X=15738, Y=9555

Button A: X+52, Y+20
Button B: X+33, Y+70
Prize: X=3537, Y=3710

Button A: X+19, Y+81
Button B: X+62, Y+24
Prize: X=3637, Y=7815

Button A: X+62, Y+19
Button B: X+27, Y+88
Prize: X=7457, Y=8743

Button A: X+26, Y+57
Button B: X+25, Y+12
Prize: X=14897, Y=11195

Button A: X+18, Y+58
Button B: X+59, Y+17
Prize: X=13930, Y=1206

Button A: X+27, Y+65
Button B: X+67, Y+18
Prize: X=3476, Y=6362

Button A: X+42, Y+22
Button B: X+23, Y+44
Prize: X=3383, Y=3840

Button A: X+67, Y+46
Button B: X+13, Y+92
Prize: X=5057, Y=7958

Button A: X+70, Y+27
Button B: X+13, Y+45
Prize: X=19215, Y=998

Button A: X+12, Y+32
Button B: X+80, Y+52
Prize: X=13748, Y=4228

Button A: X+44, Y+65
Button B: X+84, Y+26
Prize: X=10068, Y=8203

Button A: X+16, Y+62
Button B: X+38, Y+17
Prize: X=14574, Y=1989

Button A: X+73, Y+18
Button B: X+14, Y+68
Prize: X=7965, Y=5946

Button A: X+13, Y+56
Button B: X+55, Y+31
Prize: X=3014, Y=3305

Button A: X+63, Y+32
Button B: X+15, Y+54
Prize: X=2804, Y=13930

Button A: X+91, Y+15
Button B: X+22, Y+31
Prize: X=4615, Y=2540

Button A: X+75, Y+51
Button B: X+17, Y+39
Prize: X=6761, Y=6189

Button A: X+38, Y+19
Button B: X+16, Y+57
Prize: X=3630, Y=1887

Button A: X+53, Y+97
Button B: X+88, Y+27
Prize: X=5771, Y=8149

Button A: X+26, Y+44
Button B: X+91, Y+46
Prize: X=7917, Y=4650

Button A: X+64, Y+41
Button B: X+22, Y+44
Prize: X=15730, Y=12897

Button A: X+18, Y+65
Button B: X+77, Y+49
Prize: X=3001, Y=6943

Button A: X+27, Y+53
Button B: X+87, Y+51
Prize: X=6354, Y=6364

Button A: X+55, Y+24
Button B: X+23, Y+44
Prize: X=13645, Y=2480

Button A: X+49, Y+73
Button B: X+50, Y+15
Prize: X=7158, Y=7511

Button A: X+29, Y+77
Button B: X+68, Y+19
Prize: X=2208, Y=10384

Button A: X+77, Y+25
Button B: X+79, Y+92
Prize: X=4883, Y=2647

Button A: X+25, Y+47
Button B: X+49, Y+11
Prize: X=16652, Y=13412

Button A: X+27, Y+50
Button B: X+94, Y+14
Prize: X=11224, Y=5258

Button A: X+18, Y+47
Button B: X+61, Y+24
Prize: X=14224, Y=1731

Button A: X+60, Y+52
Button B: X+21, Y+92
Prize: X=5346, Y=8028

Button A: X+19, Y+91
Button B: X+43, Y+17
Prize: X=3107, Y=7323

Button A: X+44, Y+27
Button B: X+22, Y+44
Prize: X=9230, Y=2204

Button A: X+11, Y+44
Button B: X+52, Y+14
Prize: X=9983, Y=4572

Button A: X+18, Y+38
Button B: X+66, Y+33
Prize: X=2750, Y=12875

Button A: X+98, Y+30
Button B: X+37, Y+78
Prize: X=11047, Y=5982

Button A: X+59, Y+12
Button B: X+16, Y+74
Prize: X=17649, Y=4764

Button A: X+85, Y+39
Button B: X+12, Y+57
Prize: X=11562, Y=5387

Button A: X+33, Y+17
Button B: X+11, Y+26
Prize: X=1716, Y=2287

Button A: X+18, Y+48
Button B: X+67, Y+41
Prize: X=11307, Y=8905

Button A: X+32, Y+77
Button B: X+49, Y+27
Prize: X=6757, Y=8532

Button A: X+20, Y+94
Button B: X+85, Y+61
Prize: X=3665, Y=8763

Button A: X+27, Y+74
Button B: X+97, Y+34
Prize: X=5746, Y=3692

Button A: X+69, Y+32
Button B: X+11, Y+50
Prize: X=10788, Y=15288

Button A: X+13, Y+47
Button B: X+75, Y+40
Prize: X=15526, Y=3069

Button A: X+61, Y+28
Button B: X+12, Y+41
Prize: X=1784, Y=2832

Button A: X+99, Y+32
Button B: X+54, Y+75
Prize: X=3357, Y=2869

Button A: X+20, Y+80
Button B: X+90, Y+67
Prize: X=3850, Y=5145

Button A: X+93, Y+49
Button B: X+22, Y+68
Prize: X=8432, Y=7940

Button A: X+41, Y+68
Button B: X+78, Y+21
Prize: X=1883, Y=1931

Button A: X+65, Y+94
Button B: X+47, Y+13
Prize: X=8685, Y=8712

Button A: X+95, Y+32
Button B: X+20, Y+74
Prize: X=5840, Y=7550

Button A: X+57, Y+24
Button B: X+25, Y+59
Prize: X=3165, Y=5259

Button A: X+51, Y+16
Button B: X+17, Y+45
Prize: X=7333, Y=12184

Button A: X+37, Y+39
Button B: X+88, Y+12
Prize: X=6633, Y=3519

Button A: X+57, Y+76
Button B: X+83, Y+31
Prize: X=8016, Y=6386

Button A: X+39, Y+60
Button B: X+43, Y+16
Prize: X=16278, Y=4740

Button A: X+21, Y+58
Button B: X+61, Y+22
Prize: X=1172, Y=12428

Button A: X+17, Y+80
Button B: X+79, Y+12
Prize: X=3299, Y=10516

Button A: X+30, Y+73
Button B: X+65, Y+25
Prize: X=4755, Y=5175

Button A: X+39, Y+61
Button B: X+85, Y+20
Prize: X=5635, Y=5990

Button A: X+70, Y+90
Button B: X+51, Y+11
Prize: X=8687, Y=6967

Button A: X+23, Y+71
Button B: X+67, Y+18
Prize: X=13878, Y=17537

Button A: X+11, Y+39
Button B: X+66, Y+19
Prize: X=8856, Y=14264

Button A: X+21, Y+65
Button B: X+76, Y+28
Prize: X=3905, Y=15293

Button A: X+41, Y+95
Button B: X+77, Y+27
Prize: X=7755, Y=10701

Button A: X+12, Y+34
Button B: X+71, Y+51
Prize: X=6546, Y=5820

Button A: X+49, Y+14
Button B: X+11, Y+28
Prize: X=16754, Y=2846

Button A: X+35, Y+59
Button B: X+79, Y+16
Prize: X=7777, Y=5728

Button A: X+20, Y+83
Button B: X+72, Y+13
Prize: X=14860, Y=8660

Button A: X+80, Y+26
Button B: X+13, Y+52
Prize: X=4678, Y=8284

Button A: X+49, Y+73
Button B: X+52, Y+14
Prize: X=5504, Y=2678

Button A: X+81, Y+50
Button B: X+17, Y+47
Prize: X=3248, Y=851

Button A: X+33, Y+23
Button B: X+14, Y+79
Prize: X=2989, Y=6584

Button A: X+28, Y+16
Button B: X+18, Y+47
Prize: X=10356, Y=9746

Button A: X+55, Y+32
Button B: X+25, Y+83
Prize: X=7595, Y=11059

Button A: X+53, Y+75
Button B: X+55, Y+19
Prize: X=4432, Y=3742

Button A: X+72, Y+46
Button B: X+12, Y+34
Prize: X=10868, Y=14402

Button A: X+82, Y+23
Button B: X+31, Y+71
Prize: X=9710, Y=6088

Button A: X+39, Y+11
Button B: X+14, Y+44
Prize: X=4774, Y=13564

Button A: X+28, Y+11
Button B: X+47, Y+71
Prize: X=16023, Y=19770

Button A: X+61, Y+32
Button B: X+23, Y+53
Prize: X=14784, Y=17306

Button A: X+14, Y+70
Button B: X+86, Y+28
Prize: X=4536, Y=5796

Button A: X+57, Y+95
Button B: X+51, Y+23
Prize: X=7092, Y=8534

Button A: X+15, Y+38
Button B: X+22, Y+11
Prize: X=821, Y=1051

Button A: X+46, Y+19
Button B: X+46, Y+71
Prize: X=13394, Y=7425

Button A: X+50, Y+65
Button B: X+89, Y+20
Prize: X=10854, Y=5880

Button A: X+12, Y+42
Button B: X+70, Y+52
Prize: X=7354, Y=8176

Button A: X+69, Y+24
Button B: X+20, Y+58
Prize: X=17427, Y=17584

Button A: X+88, Y+71
Button B: X+26, Y+88
Prize: X=6564, Y=8379

Button A: X+63, Y+36
Button B: X+28, Y+54
Prize: X=8348, Y=9854

Button A: X+80, Y+33
Button B: X+15, Y+97
Prize: X=6465, Y=4029

Button A: X+73, Y+23
Button B: X+15, Y+29
Prize: X=1987, Y=1597

Button A: X+13, Y+57
Button B: X+75, Y+14
Prize: X=15512, Y=1558

Button A: X+69, Y+20
Button B: X+14, Y+69
Prize: X=17496, Y=17355

Button A: X+11, Y+59
Button B: X+82, Y+37
Prize: X=15363, Y=8718

Button A: X+19, Y+87
Button B: X+60, Y+64
Prize: X=3477, Y=3909

Button A: X+53, Y+14
Button B: X+12, Y+41
Prize: X=16649, Y=14282

Button A: X+20, Y+39
Button B: X+51, Y+15
Prize: X=16439, Y=15344

Button A: X+47, Y+12
Button B: X+15, Y+43
Prize: X=2929, Y=10342

Button A: X+57, Y+78
Button B: X+82, Y+13
Prize: X=10750, Y=5980

Button A: X+31, Y+59
Button B: X+32, Y+11
Prize: X=4661, Y=3737

Button A: X+73, Y+30
Button B: X+15, Y+61
Prize: X=8090, Y=7547

Button A: X+11, Y+30
Button B: X+47, Y+20
Prize: X=12889, Y=990

Button A: X+55, Y+15
Button B: X+24, Y+78
Prize: X=12768, Y=18026

Button A: X+23, Y+73
Button B: X+94, Y+11
Prize: X=3058, Y=6545

Button A: X+39, Y+42
Button B: X+12, Y+67
Prize: X=3942, Y=8301

Button A: X+62, Y+26
Button B: X+26, Y+59
Prize: X=12172, Y=9892

Button A: X+16, Y+93
Button B: X+69, Y+14
Prize: X=3234, Y=2541

Button A: X+14, Y+71
Button B: X+95, Y+78
Prize: X=8965, Y=10336

Button A: X+62, Y+15
Button B: X+45, Y+80
Prize: X=8641, Y=6030

Button A: X+39, Y+66
Button B: X+44, Y+18
Prize: X=9803, Y=482

Button A: X+17, Y+55
Button B: X+59, Y+29
Prize: X=8626, Y=4894

Button A: X+85, Y+13
Button B: X+27, Y+44
Prize: X=3056, Y=587

Button A: X+15, Y+26
Button B: X+70, Y+11
Prize: X=6810, Y=2205

Button A: X+65, Y+45
Button B: X+20, Y+91
Prize: X=2135, Y=4950

Button A: X+21, Y+84
Button B: X+81, Y+64
Prize: X=7773, Y=7692

Button A: X+90, Y+13
Button B: X+57, Y+67
Prize: X=8742, Y=3966

Button A: X+75, Y+90
Button B: X+96, Y+13
Prize: X=10920, Y=8505

Button A: X+72, Y+23
Button B: X+21, Y+61
Prize: X=18494, Y=15445

Button A: X+34, Y+35
Button B: X+63, Y+14
Prize: X=2677, Y=1281

Button A: X+51, Y+28
Button B: X+11, Y+32
Prize: X=13183, Y=16724

Button A: X+21, Y+55
Button B: X+89, Y+53
Prize: X=10743, Y=10307

Button A: X+41, Y+83
Button B: X+56, Y+32
Prize: X=5224, Y=8704

Button A: X+49, Y+12
Button B: X+17, Y+56
Prize: X=10960, Y=12140

Button A: X+11, Y+39
Button B: X+45, Y+15
Prize: X=204, Y=13706

Button A: X+50, Y+14
Button B: X+20, Y+41
Prize: X=17170, Y=14101

Button A: X+18, Y+65
Button B: X+61, Y+53
Prize: X=5010, Y=5044

Button A: X+87, Y+55
Button B: X+16, Y+36
Prize: X=5171, Y=4175

Button A: X+45, Y+15
Button B: X+15, Y+56
Prize: X=2555, Y=5984

Button A: X+53, Y+74
Button B: X+33, Y+11
Prize: X=11503, Y=270

Button A: X+20, Y+80
Button B: X+79, Y+32
Prize: X=8028, Y=5984

Button A: X+65, Y+26
Button B: X+27, Y+74
Prize: X=4313, Y=4822

Button A: X+93, Y+88
Button B: X+91, Y+15
Prize: X=14902, Y=8270

Button A: X+18, Y+44
Button B: X+49, Y+26
Prize: X=19702, Y=236

Button A: X+21, Y+59
Button B: X+30, Y+23
Prize: X=2943, Y=4530

Button A: X+21, Y+68
Button B: X+77, Y+27
Prize: X=10084, Y=11624

Button A: X+56, Y+76
Button B: X+77, Y+13
Prize: X=8925, Y=6165

Button A: X+43, Y+11
Button B: X+16, Y+33
Prize: X=14151, Y=7844

Button A: X+35, Y+71
Button B: X+45, Y+20
Prize: X=17725, Y=1191

Button A: X+11, Y+90
Button B: X+37, Y+20
Prize: X=2041, Y=2280

Button A: X+11, Y+41
Button B: X+75, Y+17
Prize: X=13429, Y=8175

Button A: X+27, Y+93
Button B: X+90, Y+25
Prize: X=5580, Y=6680

Button A: X+45, Y+15
Button B: X+21, Y+28
Prize: X=4728, Y=3109

Button A: X+14, Y+72
Button B: X+61, Y+23
Prize: X=4007, Y=5781

Button A: X+18, Y+37
Button B: X+52, Y+29
Prize: X=16434, Y=17042

Button A: X+96, Y+61
Button B: X+12, Y+80
Prize: X=1104, Y=1570

Button A: X+70, Y+32
Button B: X+12, Y+44
Prize: X=11194, Y=3032

Button A: X+28, Y+12
Button B: X+17, Y+49
Prize: X=5190, Y=3398

Button A: X+32, Y+37
Button B: X+79, Y+16
Prize: X=4841, Y=2659

Button A: X+18, Y+50
Button B: X+58, Y+26
Prize: X=16030, Y=13374

Button A: X+86, Y+75
Button B: X+17, Y+83
Prize: X=4546, Y=8055

Button A: X+36, Y+15
Button B: X+23, Y+78
Prize: X=3806, Y=6375

Button A: X+82, Y+98
Button B: X+63, Y+20
Prize: X=3336, Y=3434

Button A: X+51, Y+12
Button B: X+31, Y+43
Prize: X=1685, Y=575

Button A: X+98, Y+11
Button B: X+61, Y+55
Prize: X=12421, Y=3850

Button A: X+30, Y+92
Button B: X+81, Y+56
Prize: X=4092, Y=4468

Button A: X+55, Y+26
Button B: X+25, Y+60
Prize: X=4420, Y=4154

Button A: X+37, Y+60
Button B: X+52, Y+25
Prize: X=6709, Y=18760

Button A: X+44, Y+12
Button B: X+16, Y+47
Prize: X=2672, Y=4097

Button A: X+66, Y+13
Button B: X+19, Y+50
Prize: X=6627, Y=7057

Button A: X+36, Y+64
Button B: X+55, Y+25
Prize: X=4293, Y=10007

Button A: X+81, Y+48
Button B: X+25, Y+64
Prize: X=4692, Y=2928

Button A: X+25, Y+49
Button B: X+23, Y+12
Prize: X=17792, Y=18970

Button A: X+43, Y+21
Button B: X+17, Y+50
Prize: X=15814, Y=4176

Button A: X+60, Y+13
Button B: X+23, Y+80
Prize: X=4190, Y=10322

Button A: X+59, Y+37
Button B: X+22, Y+42
Prize: X=2171, Y=18917

Button A: X+69, Y+27
Button B: X+28, Y+53
Prize: X=8517, Y=6486

Button A: X+24, Y+71
Button B: X+66, Y+44
Prize: X=2784, Y=7026

Button A: X+24, Y+47
Button B: X+95, Y+12
Prize: X=8245, Y=1701

Button A: X+51, Y+99
Button B: X+94, Y+43
Prize: X=5124, Y=3252

Button A: X+56, Y+12
Button B: X+23, Y+56
Prize: X=7144, Y=15108

Button A: X+86, Y+15
Button B: X+70, Y+66
Prize: X=904, Y=588

Button A: X+68, Y+47
Button B: X+32, Y+85
Prize: X=8252, Y=11363

Button A: X+44, Y+14
Button B: X+12, Y+54
Prize: X=244, Y=3370

Button A: X+32, Y+11
Button B: X+14, Y+34
Prize: X=8614, Y=4150

Button A: X+54, Y+32
Button B: X+14, Y+68
Prize: X=5058, Y=4072

Button A: X+27, Y+11
Button B: X+38, Y+68
Prize: X=11766, Y=7974

Button A: X+45, Y+78
Button B: X+68, Y+34
Prize: X=3537, Y=5376

Button A: X+13, Y+51
Button B: X+93, Y+93
Prize: X=4098, Y=6834

Button A: X+65, Y+17
Button B: X+37, Y+48
Prize: X=6627, Y=5029

Button A: X+11, Y+70
Button B: X+59, Y+72
Prize: X=5816, Y=9700

Button A: X+37, Y+57
Button B: X+84, Y+12
Prize: X=10321, Y=4629

Button A: X+60, Y+20
Button B: X+25, Y+61
Prize: X=8045, Y=15953

Button A: X+46, Y+31
Button B: X+32, Y+72
Prize: X=5884, Y=6134")