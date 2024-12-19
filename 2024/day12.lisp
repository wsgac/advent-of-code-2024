(in-package #:advent-of-code-2024.day12)

(defun parse-input (input)
  (util:parse-string-into-array input))

(defparameter *deltas* '((-1 0) (0 1) (1 0) (0 -1)))

(defparameter *diag-deltas* '((1 1) (-1 -1) (1 -1) (-1 1)))

(defun perimeter-contribution (arr row col)
  "Given an array `arr` and `row` and `col`, return a list of neighboring
fields of a different kind than the given one."
  (loop
    with ch = (aref arr row col)
    for (dr dc) in *deltas*
    count (not (ignore-errors
                (char= ch (aref arr (+ row dr) (+ col dc)))))))

(defun get-neighbors (arr row col)
  (loop
    with ch = (aref arr row col)
    for (dr dc) in *deltas*
    if (ignore-errors (char= ch (aref arr (+ row dr) (+ col dc))))
      collect (list (+ row dr) (+ col dc))))

(defun get-distinct-plists (h neighbors)
  (loop
    for neighbor in neighbors
    for plist = (gethash neighbor h)
    if plist
      collect plist into plists
    finally (return (remove-duplicates plists :test #'equal))))

(defun find-groups (arr)
  (loop
    with groups = nil
    for row from 0 below (array-dimension arr 0)
    do (loop
         for col from 0 below (array-dimension arr 1)
         for neighbors = (get-neighbors arr row col)
         do (setf groups (loop
                           for g in groups
                           if (intersection g neighbors :test #'equal)
                             append g into merged
                           else
                             collect g into others
                           finally (return (cons (cons `(,row ,col) merged) others)))))
    finally (return groups)))

(defun group-cost (arr group)
  (loop
    for (r c) in group
    count 1 into area
    sum (perimeter-contribution arr r c) into perimeter
    finally (return (* area perimeter))))

#+(or)
(defun sides (h)
  (+ (hash-table-count h)
     (loop
       for v being the hash-values of h
       sum (loop
             for (x y) on (sort v #'<)
             when (and y (> (- y x) 1))
               sum 1))))

(defun corner? (arr r c dr dc)
  (let ((ch (aref arr r c)))
    (or
     ;; convex
     (not (or (ignore-errors (char= ch (aref arr r (+ c dc))))
              (ignore-errors (char= ch (aref arr (+ r dr) c)))))
     ;; concave
     (ignore-errors
      (and (char/= ch (aref arr (+ r dr) (+ c dc)))
           (char= ch (aref arr r (+ c dc)))
           (char= ch (aref arr (+ r dr) c)))))))

(defun point-corners (arr r c)
  (loop
    for (dr dc) in *diag-deltas*
    count (corner? arr r c dr dc)))

(defun corners (arr group)
  (loop
    for (r c) in group
    sum (point-corners arr r c)))

(defun group-cost-2 (arr group)
  (let ((area (length group))
        (sides (corners arr group)))
    (* area sides)))

#+(or)
(defun group-cost-2 (arr group)
  (loop
    with hr = (make-hash-table)
    with hc = (make-hash-table)
    for (r c) in group
    for ch = (aref arr r c)
    count 1 into area
    unless (ignore-errors (char= ch (aref arr (1- r) c)))
      do (push c (gethash r hr))
    unless (ignore-errors (char= ch (aref arr (1+ r) c)))
      do (push c (gethash (1+ r) hr))
    unless (ignore-errors (char= ch (aref arr r (1- c))))
      do (push r (gethash c hc))
    unless (ignore-errors (char= ch (aref arr r (1+ c))))
      do (push r (gethash (1+ c) hc))
    finally (return (* area (+ (sides hr) (sides hc))))
    ;; finally (return (list :char ch :area area :sides (+ (sides hr)
    ;;                                                     (sides hc))))
    ))

(defun plist< (p1 p2)
  (if (= (getf p1 :area) (getf p2 :area))
      (< (getf p1 :sides) (getf p2 :sides))
      (< (getf p1 :area) (getf p2 :area))))

(defun problem-1 (&key (input *input-part-1-test*))
  (loop
    with arr = (parse-input input)
    for g in (find-groups arr)
    sum (group-cost arr g)))

(defun problem-2 (&key (input *input-part-1-test*))
  (loop
    with arr = (parse-input input)
    for g in (find-groups arr)
    sum (group-cost-2 arr g)))


(defparameter *input-part-1-test*
  "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defparameter *input-part-2-test*
  "AAAA
BBCD
BBCC
EEEC")

(defparameter *input*
  "KKKKKKCVCCCCCCEEEEEEJJJJJJJJLLLLLLLLYYJJJJJJJJTGJJGGGWWWZZZZZZGGGGGGGJJZZZZZZTTTTTTTAAAAAAAAAUUUUGGGZZZZZZZSSSSSSSSSHHHHHHHHHPPPRRRRGGGGGGGG
KKKCCCCCCCCCCCEEEEEEEJJJJJJLLLLLLLLLYYYJKJJTTTTGGGGGGWWWGZZZZZZZGGGGCZZZZZZZZZTTTTTTAAAAAAAAAUUUUUUUZZZZZZSSSSSSHSSSHHHHHHHHHPPPRRRRRGGGGGGG
KKKCCCCCCCCCCCCEEEEEEJJJJJJLLLLLLLLLLLLKKJJTTTTGGGGGWWGGGGZZZZZGGGGGCZZZZZZZZZTTTTTAAAAAAAAAUUUUUUUUXZZZZZSSSSSSHHHHHHHHHHOOHHPRRRRRRGGGGGGG
KCKCCCXCCCCCCCCCCEEEEJJJJJJLLLLLLLLLLLLTTTTTTTTGGGGGGGGGGZZZZRZRGGJGCZMMMMZZZTTTTTTAAAAVAVVAAUUUUDUUZZZZZKBBBSHHHHHHHHHHHHOOOPPRRRRRRGGGGGGG
KCCCCCCCCCCCCCCCCEEEEEJJJJLLLLLLLLLLLLLTTTTTTTTTGGGGGGGGGZZZRRRRRGRRZZMMMMZZZTTTTTTTAAAVVVVUUUUUUUUUUZZZZZPBVBBBPHHHHHHHHHOOOPPRRRRRRRRGGGSG
KKQQQQCCCCCCCCCCCEEEEJJJJLLLLLLLLLLLLRLTTTTTTTTTMGGGGGGNGZZZRRRRRRRRMMMMMMMMMTTTTTTTAAAAVVVUUUUUUUXZZZZBZZZBBBBPPHHHHHHHHHHHOOHHRMRRRRRGGGGG
KKQQQQCCCCCCCCCCEEEEEECCZZZLLLLLLLLLRRTTTTTTTTTTMMMMGGNNZZZZRRRRRRRRMMMMMMMMMTTTTTAAAAAAVVVHHHHUUZZZZZZBBBBBBBGGGMHHHHHHHHHHOOHHMMMRMRRGGGRR
KQQQQQCCCCCCCCCEEEEEEEEEZZZLLLLLLLLLMTTKTTTMMBTTTMYYIGNNNNZZRRRRRRRRMMMMMMMMMTTTTTTMMAHAVVVHHHHUUUZZZZZBBBBBBGGGGGHHHHHHHHHHHOHSMMMMMMRRRRRR
QQQQQQQCCCCCCAAEELEEEZZZZZZLLLLLLLMMMMTTTTMMMTTTMMYYYYYYNNYZZRRRRRRRRRMMMMMMMTTTTMMMMHHVVVVVHHHHHZZZZBBBBBGBBGGGGGHHHHHHHHHHHHHMMMMMMRRRRRRR
XQQQQQQQCCNCCADDDDEEEZZZZZZLLLLEEMMMMMMTTTMMMTTTMMYYYYYYYNYYDYGRRRRRRRMMMMMMMMTTTTMMMHHHHHHVHHHHHZHZZZBBBBGGGGGGGGHHHHHHHHHLHHHMMMMMMRRRRRRR
XQQQQQQQQCCCCAADFFFFFZZZZZLLLLLEMMMMMMMMMMMMMTTTMYYYYYYYYYYYYYYRRRUUUUUUUUUMMMMTTMMMMHHHHHHHHHHHHZHHZZBYGGGGGGGGGHHHHHHHLLLLLLHKMMMMMRRRRRRR
QQQQQQQQQQCCAADDDFDFFZZZZLLLOMMMMMMMMMMMMMMMTTIMYYYYYYYYYYYYYYYRRRUUUUUUUUUMMMMMMMMMMMHHHHHHHHHHHHHHYYYYYYGGGGGGGGHLLLLLLLLLLIMKMMMMRRRRRRRR
QQQQQQQQQQDDDDDDDDDFFDDDNNNNOOMMMMMMMMMMMMMMMMMMMYYYYYYYYYYYYYRRRRUUUUUUUUUMMMMMMMMMMMHHHHHHHHHHHHHHYYYYYYGGGGGGGGGLLLLLLLLLLIMMMMMMRRRRRRRR
QQQQQQQQQQAADDDDDDDFFDDDNOOOOOMTTMMMMMMMMMMMMMMMMYYYYYYYYYYYUUUUUUUUUUUUUUUMMMMMMMMMMMHHHHHHHHHHHHHHYYYYYYGGGGGGGGGGLLLLLLLLOIMMMMMMRRRRRRRR
QQQQQQQQQQADDDDDDDDDDDDDNOOOOOOOTTYYMYMMMMMMMMMMYYYYYYYYYYYYUUUUUUUUUURIUMMMMMMMMMMMMMMHHHHHHHHHHHHYYYYYYYYGGGGGGGGGGLLLLOOOOIIMRRRRRRRRRRRR
QQQQQQQQQQDDDDDDDDDDDDDDDDOOOYYOYYYYYYMMMMMMMMMMMYYYYYYYYYYYUUUUUUUUUUUIUUUMUMUMMMMMMMMHHHHHUHHHHHHHYYYYYGGGGGGGGGGGGWLLOOOOOOMMMRRRRRIIRRRR
QQQQQQQQQBBDRDDDDDDDDDDDEEEEEYYKKYYYYYMMMMMMMMMMBYYYYYYYYYYYUUUUUUUUUUIIUUUUUUUMMMMMMMMHHHUUUUUUUHHHGGGGGGGGGGGGGGGGGWLOOOOOOOMMMRRIRIIRRIII
QQQQQQQWWBBDDDDDDDDDDDDDDDDEYYYYYYYYYYMMMMMMMMMBBBYYYYYYYYYYUUUUUUUUUUUUUUUUUUUMMMMMMMHHHHUUUUUUUUUUUGGGGGGGGGGGGGWGGOOOOOOOOOIMRRIIIIIIIIIC
QQQQQQWWWWBDDDDDDDDDDDDDEEEEYYYYYYYYYYYMMMMMMMBBBBBYYYYYYYYUUUUUUUUUUUUUUUUUUUUMMMMMMMHHHHUUUUUUUUUUUUGGGGGGGGGGGGWGGWWWOOOOOORRRIIIIIIIIICC
QQQQQQQWWWBBDDDDDDDDDDDDDEEYYYYYYYYYYYYMMMMMMMBBBBBBBYBYYYYUUUUUUUUUUUUUUUUUUUUMMMMMMMHHHHUUUUUUUUUVUUGGGGGGGGWWWWWWWWWWWWWOORRRRRRIIIIIIIIC
FFFQQQQWWWBBDDBBBDDWDPDWDEEEYYYYYYYYYYYMMMMBBBBBBBBBBBBBBYYUUUUUUUUUUUUUUUUUUUUMMMMMMMHHHHAAUUUUUUIUGGGGGGGGGGGGWWWWWWWWWWWOYYRRRRMIIIITIRRC
FFFQQQQWWWBBDDBBEEDWWWWWWWYYYYYYYYYYYYBMMMBBBBBBBBBBBZBPPPPPPPPPUUUUUUUUUUUUUUUPMSMMNMHHHHHHUUUUUUUUGGGGGGGGGGGGGGWWWWWWWWWOOYYYYRMIIIITRRRC
FFFQLQQQWWBBBBBBEEWWWWWWWWYYYYYYYYYYYYBBBBBBBBBBBBBZZZZZPPPPPPPPUUUUUUUUUUUUUUUMMMMMPPHHHHHUUUUUSUUUGGGGGGGGGGGGGGWWWWWWWWWYYYYYRRRIIIIRRRUR
FFFFFFFWWWWBBBEBEEEWWWWWYYYYYYYYYYYYYYBBBBBBBBBBBBBZZZZZPPPPPPPPUUUUUUUUUUUUUUUPPPPPPPPEHHUUUUUHUUUUJGGGGGGGGGGGGGWWWWWWWWWYYYYYRRRIRRRRRRRR
OOOOOOOOWWWBBEEEEEWWWWWWWWWYYYYYYYYYYBBBBBBBBBPPPPZZZZZZZPPPPPPPUUUUUUUUUUUUPPPPPPPPPPPEHHUGUUHHHHUUGGGGGGGGGGGGGWWWWWWWWWWYYRRRRRRRRRRRRRRR
OOOOOOOOBBBBBEEEEEEWWWWWWWWWWMYYYYYYYGBWWWWBBBPZZZZZZZZZZPPPPPPPUUUUUUUUPUUUPPPPPPPPPPPHHHUUUUUUHHUHGGGGGGGGGGGGGGWWWWWWWWWWRRRRRRRRRRRRRRRR
OOOOOOOOOOOOOTEEEEEWWWWWWWWWWWYYZYYYYYYWWWWWBBPZZZZZZZZZZZZPPPPPUUUUUUUUUUUUUUUUUPPPPXPPHUUUUUUHHHHHGGGGGGGGGGGIWWWWWMMWWWWCCRRRRRRRRRRRRRRR
OOOOOOOOOOOOOOOMEEEWWWWWWWWWWWWWWWEYYYYWVWWWWWPZZZZZZZZZZZZPPPPPUUUSUUUUUUUUUUUUUPPPPPPPNUUUUUUHHHHHHGGGGGGGGGIIIWWWWWMMMWWRRRRRRRRRRRRRRRRR
OOOOOOOOOOOOOOOOEEEEWWWWWWWWWWWWWWEYYYWWVVWWWWPZZZZZZZZZZZZPPPPPUUSSSSUUUUUUUUUUUPPPPPPNNUUUUUUHHHHHHGGGGGGGGGIIWWWWWWMMMMMRRRRRRRRRRRBRRRRR
OOOOOOOOOOOOOGOEEEEEWEEWWWWWWWWWWWWZYYYVPPPPPPPZZZZZZZZZZZZPPPPPDUSSSSSSUUUUPPPPPPPPPPPNNNNUUUUHHHHHHGGGGGGGGGIIWWWWMMMMMMMRRRRRRBBRRBBBRRBB
OOOOOOOOOOOOOOEEEEEEEEEZWWWWWWWWWWWWWYMVPPPPPPPZZZZZZZZZZZZUUUUUDDDSSSSSSUUUPPPPGPPPPPNNNNNUUHHHHHHHHGDDDDGGGZZZZWWMMMMMMMMRRRROROBBBBBBBBBB
OOOOOOOOOOOOOEEEEEEEEZVVVVVVVVVWWVVVVVVVPPPPPPPZZZZZZZZZZBBBBBUUDDDDSSSSSUUUPPPPGIIIIIIHNNUUHHHHHHHHGGDDDDGGGZZZZWWMMMMMMMMMMMOOOOOBBBBBBBBB
OOOOOOOOOOOOOEEEEEEEZZVVVVVVVVVWWVVVVVVVPPPPPPPZZZZZZZZZZBBBBWDUDDDDDSSSSUUUPPGGGIIIIIIHNNNUUHHHHHHHDDDDDDDGGZZZZMMMMMMMMMMMMMMOOOOBBBBBBBBB
BBBBOOOOOOOOOOOOZZZZZZVVVVVVVVVWWWVVVVVVPPPPPPPPPPBBBBBBBBBBBDDDDDDDSSSSSUUUCAGGGIIIIIIHHHHHHHHHHHDDDLDZZDDGZZZZZMMMMMMMMMMOMOOOOBBBBBBBUBBB
BBBBOOOOOOOOOOOOUZZZZZVVVVVVVVVWWWWVVVVVPPPPPPPPPPBBBBBBBBBBBDDDDDDDSSSSAUUUAAGGGGIIIIIHHHHHHHHHHHDDDDIZZZZGZZZZZZZZMMOMMMMOOOOOEELBBBBMUUBB
BBBBOOOOOOOOOOOOZZZZZZVVVVVVVVVWWWWVVVVVPPPPPPPPPPBBBBBBBBBBBDDDDDDDDDDSAUUUAAGGGGIIIIIHHHHHHDDDHHDDDDDZZZZZZZZZZZZZGGOOOOLOOOOOOELLLBDUUUBU
BBBBOOOOOOOOOOOOZZZZZZVVVVVVVVVWWWWWVVVVPPPPPPPPPPBBBBBBBBBBBDDQDDDDDDTAAUUUAAAGGGIIIIIHHHHHHDDDDDDDDLLLZZZZZZZZZZZGGOOOOOOOOOOOEELELLUUUUUU
BBBBOOOOOOOOOOOOZZZZZZVVVVVVVVVWWWVVVTVVVVVVVVVLLLLBBBBBBBBBBBBBTTTTTTTLLAAAGGGGGGIIIIIVDDDDDDDDDDDDDLLLZZZZZZZZZZZOOOOOOOOOOOOOEELELLLUUUUU
BBBBOOOOOOOOOOOOZZZZZZZZZZZWWWWWWWVVVTTVVVVLLLLLLLLBBBBBBBBBBBNNTTTTTTLLAAAAAGGGFFIIIIIDDDDDDDDDDDDDDLLLNZZZZZZZZUUOOOOOOOOOLLLOEEEEELLLLUUU
BBBBOOOOOOEEEZZZZZZZZZZZZZZWWWWWWWBWTTTVVVVLYLLLLLBBBBBBBBBBBBNTTTTTTTTLLLAAAGGGFFIIIIIFDDDDDDDDDDDDDDDDNZNZZNZZZZUUUUOOOOOOOZLEEEEEEELLLULU
BBBBOOOOOOEEEVZZZZZZZZZZZZNWWWWWWWWWTTTTVVVLYYYYYLBBBBBBBBBBBBTTTTTTTTTLLLLLAGGFFFFFFFFFDDDDDDDDDDDDDDDNNNNNNNZZRZUUOOOOOOOOOLLREEEEELLLLLLU
BBBBBBVOVVVNNVKZZZZZZZZZZZNNWWWWWWWWWTTTTYVVYYYYYLBBBBBBBBBBBBBTTTTTTTTLLLLLLTTTFFFFFFFFDDDDDDDDDDDDDDDDNNNNEEEZZUUUOOOOOOOLLLLLEEEEEEELLLLU
BBBBBVVOVVVVVVZZZZZZZZZZZZZNWWWWWWWWTTTTTYYYYYYYYLLLLWWWBBBBBBMMTMTTTTTLLLLLLTTTFFFFFFFFDDDDDDDDDDDDDDDNNNNNEEEEEUUUOGOLLLOLLLLLHEEEEEELLLLU
BBBBBVVVFVVVVVVVZZZZZZZZRRZNWWWWWWWWWTTTYYYYYYYYYLLLLWWWWWBBBMMMMMMTTTTLLLLLTTTTFFWWFFEFDDDDDDDDDDDDDDNNNNNEEEEEEEUEOOOLLLLLLLLLEEEEEEELLULU
CCBBBBVVVVVVVVVVVKKCZZZRRRVVVVWVVWWWWKTTTTTYYYYYYLLLWWWWWWWWWWWMMMMWWLTLLLLTTTTTTUTWWWEEEEDDDVDDDDDDEETTNNEEEEEEEEEEESLLSLLLLLLEEEEEEEEEEEEU
CCCCBUUUVVVVVVVVVKKCZZRRRCVVVVVVWWWWEKKTTTYKYYYYYWWWWWWWWWWWWWWWWWWWWLLLLLLTTTTTTTTWWWEEEEEDEEDSSDDDETTTEEEEEEEEEEEEESSSSSLLLLLLLEEEEEEEUUUU
CCCCCVVVVVVVVVCPCCCCCZCCCCCVVVVVWWVEEKKKKTYYYYYYYYWWWWWWWWWWWWWWWWWWWWLLLLTTTTTTTTTWWWWEEEEEEESSDDDDDTZTEOEEEEEEEEEEEEEEVSLLLLLLLLEEEEEEUUUU
CCCCCCCVVVVVVVCPCCCCCCCCCCHCVVVVVVVKKKKKTTYYYYYYYYYYWWWWWWUUWWWWWWWWWUQLLQQJTTTWWTTTWWWEEEEEEEESSSSSDTTTTEEEEEEEEEEEEEEEVVLLLLLLLLEEEEUUUUUU
CCICCCCVVVVVVVCCCCCCCCCCCCCCCVVVVVVKKKKKKEYYYYYYYWWWWWWSSSSUWWWWWWWWGQQQQQQQQTTWWTWWWWEEEEEEEEESSSTTTTTTTIIEEEEEEEEEEAEVVVVNLLLLEEEEUUUUUUUU
CCICCCCVVVVVVVCCCCCCCCCCCCCCCVVVVVVVVKKKEEEYYYYYYYYWWWSSSSSUWWWWWWWWWWQQQQQQTTTTWWWWWWWEEEEESSSSSSTTTTTTIIIIEEEEEEEEEVVVVPNNLLLLLUUUUUUUUUUU
IIIICCCCVVVVVCCCCCCCCCCCCCHHVVVVVVVVKKKKEEEEYYYYYYYYSASSSSSSWWWWWWWWQQQQQQQQQQQQQWWWWWWEEEESSSSSSSSTTTTIOIIIIEEEEEEEEEVVVPPNLLULUUUUUURRRRRR
IIIIIICVVVVVCCCCCCCCCCCCCCCVVVVVVVVVVKKEEEEEYYYYYYYZSSSSSSSSSWWWWWWWQWQQQQQQQQQQQQWWWWWEESSSSSSSSSTTTDIIIIIIIEIEEEEDELPVPPPRRUUUUUUUUUURRRRR
IIIIIIVVVVFOCCCCCCCCCCCCCCVVVVVVVVVVVNEEEEEEYYYYYYYSSSSSSSSSSTWWWWWWWWWWQQQQQQQQQQWWWWWWESSSSSTSSSSTDDDRIIIIIIIIEEEEELPPPPPRRRRUUUUUUURRRRRR
IIIIIVVVVFFFCCCCCCCCCCCCNBHVVVVVVVVVVNEEEECCYCRRYRRSSSSSSSSSBSWWWWWWWWWWWMQQQQQQQQQRRRRRESSSSSTTSSSSIDDDIIIIIIIEEEEEEEPPPPRRRRRUUFFUFFRRRRRR
IIIIIVVVVTFFCFCCCCCCCCCNNBHHHHVVVVVVNNNNNNCLCCRRRRRRSSSSSHHHHHHWWWWWWWWWMMMQQQQQQQRRRRRRSSTTTSTTSSSSIDLDIIIIIIIIEEEEPEPPRRRRRRRUUFFFFRRRRRRR
IIIIIIFVVVLFFFFCCCCCCCCNBBBBVVVVVVVVNNNNCNCCCCCCRRRSSSSSSHHHHHHJWWWMWWWMMMMQQQTQQQRRRRSSSSTTTTTTTSSSIDDDIIIIIIIIIEEEPPPPPRRRRRRRRFFFFFRRRRRR
IIIIIIFFVFFFFFFCCFCCCVVBBBBBBBVVHVVNNNCCCCCCCCCCRRRSSSHHHHHHHHHJWWWMSWWMMMMMQQTQRRRRRRSSSSTTTTTTSSIIIIIIIIIIIIIPPPPPPPPPPRRRRRRRFFFFFFRRRRRR
IIIIIFFFFFFFFFFFFFCCCCBBBBBBBBBBBBBBCCCCCCCCCCCCRRRSSSHHHHHHHHHJWWWMMMMMMMMTTTTIITRRRRRRTTTTTTTTIIIIIIIIIIIIIIIPPPPPPPPPPRRRRRRRRFFFFFRRRRRR
IIIIIIFFFFFFFFFFFFCCCYYYBBBBBBBBBBBBCCCCCCCCCCCCRSRSSSHHHHHHHHHJWWWMMMMMMMTTTTTTTTTRRRDDTZTTTTTKCIIIIIIIIIIIIIIPPPPPPPPPRRRRRRRRFFFFRRRRRRRR
IIIFFFFFFFFFFFFFFFFCCYYYBBBBBBBBBBBCCCCCCCCCCCCCSSSSSSHHHHHHHHHOOMMMMMMMMMMMTTTTTTVVRMDZZZZTKKKKKIIIIIIIIIIIIIIPPPPPPPPPRRRRRRRRFWFRRRRRRRRR
NIISSFFFFFFFFFFFFFFCCYYYBBBBBBBBBBCCCCCCCCCCCCSSSSSSSSHHHHHHHHHOOOMMMMMMMMMMMJJJJJJJJMDZZZZZZKYKKIIKKIIIIIIIIIIPPPPPPPPRRRRRRRRRFFFRRRRRRRRR
NTTTSFFFFFFFFFFFFFCCCYYYBBBBBBBBBBBCCCCCCCCCCCSSSNSNSSHHHHHHHHHOOOMMNMMMMMMMMJJJJJJJJMMZZZZZZZKKKIIKKKIIIIIIIIIIPPPPPPPPRRRRRRRRRRRRRRRNNRRR
TTTTSFFFFFFFFFFFFFFCYYYYYBBBBBBBBBBBCCCCCCCCCCCCNNNNNNHHHHHHHHHOOONNNMMMMMMMMJJJJJJJJMMMMMZZZKKKKKKKKKKIIIIIIIPPPPPPPPPRRRRRRRRRRRRRRNNNNNRR
TTTTSTTVVFFFFFFFFFFYYYYYBBBBBBBBBBBBCCCICCCCCNNNNNNNNSHHHHHHHHHONNNNNNNNNMMMJJJJJJJJJMMMZZZZKKKKKKKKKKKIIIMMPPPPPPPPPPPPRRRRUURRRRRRRNNNNNNJ
TTTTTTFVVFFFFFFFFFFYYYBBBBBBBBBBBBBBBBIICCCCCNNXXXNNNNHHHHHHHHHOONNNNNNNNMMMMMMVMMMMMMMMZZZZZZZKKKKZKKKIIMMMMPPPPPPPPPRRRRRRUURRRRHRNNNNNNNN
GTTTTTTVVFFFFFFFFFFFWWWUBVBBBBBBBBBBIIIIICCCCCNXXXNXNNHHHHHHHHHOOXNNNNNNNNMMMEMVMMMMMMMMMZZZZZZKZKKZKKKIIMMMMMPPPPPPPPRRRRRUUURRRRNNNNNNNNNB
GTTTTVVVVFFFFAAAWWWWWWWWWVBBBBBBBBIIIIIIITCCCXXXXXXXXNNHHHNNNOOONXNNNNNNNMMMEEMMMMMMMMMMMZZZZZZZZZZZKZZZMMMMMMPPPPPPPPPZZRZZUUUUVRNNNNNNNNNN
TTTTTTVVVVBBFAAAAAAWWWWWWBBBBBBOZZZIZIIIITQCCCXXXXXXXNNNNNNNNOOONNNNNNNNNNEEEMMMMMMMMMMMMMZZZZZZZZZZZZZZMMMMMMMMPPPPPPPPZZZUUUUUVVNNNNNNNNNN
TTTTTVVVVVBBFFAAAAAAAWWWWAABBBZZZZZZZIIIITTEEEXXXXXNNNNNNNNNNNNNNNNNNNNNNEEEEEEMMMMMMDMDMMZZZZZZZZZZZZZZMMMMMMMMUUUPPPPPZZZUUUUUUVVNNNNNNNNN
TTTTVVVVVBBEEEAAAAAAWWWWWAAZBBZZZZZZZIIIITIIEUUXXXXNNNNNNNNNNNNNNNNNNNNNNEEEEEEEMMMMDDDDDZZZZZZZZZZZZZZZMMMMLMMMUUUUPPPPUZZUUUUUUUNNNNNNNNNN
TTETVVVVVBBBEEEEEAAAAAWAAAAZZZZZZZZZZIIIIIIEEEUXXXXENENNNNNNNNNNNNNNNNNNNEEEEEEEMMDDDDDDZZZZZZZZZZZZZFFFLLLLLMMMUUUUPPPUUZZCUUUUUUNNNNNNNNNN
TPVVVVVVVBEEEEEEAAAAAAAAAAAZZZZZZHHHHZYYYIIEEEEOXXEEEENNNNNNNUUJNNNNNNNNNNNEEEEEEDDDDDDDDDZZZZZZZZZZZFFFFLLLLMMUUUUUUUUUUZZCUUUUUUUNNNNNNNNN
PPVVVVVVVVEEEEEEAAAAAAAAAAAAZZZHHHHHHHYYYIUEEEEXXXXEEENNEENNUUUUUUNVVNNENNNEEEEEEEEDDDDDDDZZZZZZZZZZFFFFFFLLLLMMUUUUUUUUCCZCUUUUUUUUUNNNNNNN
LPPVVVVVVLLLHHEEAAAAHHHHAAHZZZZHHHHHHHYYYYUUEEEEXXXEEEEEEENUUUUUUUNNNNEEEEEEEEEEEEENDDDDDDSZZZZZZZZZFFFFLLLLLLMUUUUUUUCUCCCCCCUUUUUCNNNNNNNN
LLPVLVVLLLLLLLQYAAAHHHHHHAHZDZHHHHHHHHMYUUUEEEEEEEEEEEEEEEUUUUUUUUUUUEEEEEEEEEEEEEENNDDDDDZZZZZZZZZZZFFFLLLLLLLUUUUUUUCCCCCCCCUUUUUUUUNNNNNN
LLNNLLLLLLLLLLYYYYHHHHHHHHHHZZHHHHHHHHMMUUUUUUUUEEEEEEEEEUUUUUUUUUUUUUEEEEEEEEEEEENNNNDDDDZDDZZZZZZZFFFFLLLLLLUUUUUURULCCCCCCCCUUUUUUUUNNNNN
LLNNNLLLLLLLLYYYYYYHHHHHHHHHZZHHHHHHHHUUUUUUUUYUEEEEEEEEEEUUUUUUUUUUUUEEEEEEEEEEOENNNNNNDDDDDZZZZZZFFFLLLLLLLFFSUUULUUCCCCCCCCCCUUUUUUUNNNNN
LLNZZLLLLLLLLYYYYYYYHHHHHHHHHHHHHHHHHHUUUUUUUUUUEEEEEEEZEEUUUUUUUUUUDDEEEEAAAOOOOENDDDNDDDDZZZZZZZFFFLLLLLLLLFFSULLLLUCCCCCCCYYCUUUUUULNMNNN
LLLLLLLLLLLLLLLYYYYYYHHHHHHHHHHHHHHHHHUUUUUUUUEEEEEEFFUUUUUUUUUUUUUUDDDEEEAAAOOOODDDDDDDDDDZZYZZZZZFFLLLLLLLLFFFFLLLLLCCCCCCCCYYYUUUUUUNNNFN
LLLLLHLLLLLLLLLYYYYYYHHHHHHHHHHHHHHHHHUUUUUUUUEUQEEEUUUUUUUDDDUUUUDDDDDEEEAAAOOOODDDDDDDDDDZZYZZZFFFFFFLLLLLFFFFFFLFLLLCWUCCCYYYYUUUUFNNNFFF
LLLLLHLLLLLLLLYYYYYYYYYHHHHHHGGGGGGGGGUUUUUUUUUUUEEUUUUUUUUDDDUUDDDDDDEEEEAAAOOOODDDDDDDDDDDWWWHWFFWWWWLLLHLLLFFFFFFFFLCWWWZCYYYUUUUFFFFFFFF
LLLLLNLLLLLLLLYYYYYYYYYYHHHHHGGGGGGGGGUUUUUUUUUUUUZUUUUUUUDDDDDDDDAAAAAAAAAAAOOOODDDDDDDDDDDDDWWWWFWWWVVHHHHLLHFFFFFFFLLWWWWWWYYUUUUFFFFFFFF
LZLNNNLLLLLLZZZYYYYYYYYYYYYHHGGGGGGGGGGGGUUUUUUUUZZUZUUUUUUDDDDDDDAAAAAAAAAOOOOOODDDDDDDDDDDDWWWWWWWWWVHWHHHHLHFFFFFFFLWWWWYYYYFFFFFFFFFFFFF
LZZZNZZLLLLLZZZYYYYYYYYYYHHHHGGGGGGGGGGGGGUUNUUUZZZZZUUUUZZDDDDDDDAAAAAAAAAOOOOOOOODDDDDDDDDDWWWWWWWWHHHHHHHHHHHFFHHHHWWWWWWWWWWWWFTTTTFFFFF
ZZZZZZZZLLLLZYYYYYYYYYYYHHHHHGGGGGGGGGGGGGUNNNUUUZZZZZZZZZZDDDDDDDDDDDDDOOOOOOOOOOODDDDDDDDDDWWWWWWWHHHHHHHHHHHHHHHHHHHHWWWWWWWWWWWWTTTFFFFF
ZZZZZFZZZZZZZZYYYYYYYYYYSSHHHGGGGGGGGGGGGGNNNNUUUUZZZZZZZDDDDDDDDDDDDDDDDOOOOOOOOOODDDDDDDDDDWWWWWWWWHHHHHHHHHHHHHHHHHHWWWWWWWWWWWWWTTTTFFFF
DFFFFFFZZZZZZZYYYYYYYYYSSHHHHGGGGGGGGGGGGGNNNNUUUZZZZZZZZZZDDDDDDDDDDDNDLLOOOOOOOOODDDDDDDDDDDWWWWWWWWWHHHHHHHHHHHHHHHTIWWWWWWWWWWWWWTPFFFFF
DDDFFFFZZZZMMZZMMYYYYYYYSSSHHGGGGGGGGGGGGGNNNNNNUZZZZZVVVVVVVVVDDDDDDDDDLLLLOOOOPOODDDDDDDDDDDDWWWWWWWWHHHHHHHHHHHHHHHTTTTTWWWWWWWWGFFFFFFFF
DDDFFZFZMMMMMMMMMMMYYYYYSSSSSGGGGGGGGGGGGGNNNNNNUZZZZZVVVVVVVVVVVVDDDDDLLLLLLLLLLDDDDDDDDDDDDKDWWWWWWWHHHHHHHHHHHHHHHHTTTTTTWWWWWWWWFFFFFFFF
DDDFFZZZMMMMMMMMMMMYYYYTLLLLLGGGGGGGGGGGGGNNNNNNNVVVVVVVVVVVVVVVVVDDDDDLLLLLLLLLLLLDDDDDDDDDKKDWWWWWWWHHHHHHHHHHHHHHHHTTTTTTWTWWWWWWFFTFFFFF
DDDDFZZZMMMMMMMMMMMMYTTLLLLLLLLLGGGGGGGGGGNNNNNNNVVVVVVVVVVVVVVVVVDLDLLLLLLLLLLGGGGDDDKKKKKKKKDDWWWWWZZHHHHHBHHHHHHHTHTTTTTTTTTWWWWWWFTFFFFF
DDDDZZZMMMMMMMMMMMTTTTTLLLLLLLLLGGGGGGGGGGGGGGNNNVVVVVVVVVVVVVVVVVDLLLLLLLLLLGGGGGGGDGMMKKKKKKKKKWWWZZWZHHHZBHHHHHHJTTTTTTTTTBTWSSSSWTTFFFFF
DDDZZMMMMMMMMMMTTTTTTTLLLLLLLLLLGGGGGGGGGGGGGGNNNVVVVVVVVVVVVVVVVVLLLLLLLLLLGGGGGGGGGGGGLLKKKKKKKWWWZZZZZZZZZHBBHJJJTTTTTTTTTTTSSSSSSTTFFFFF
DDDDDDMMMMMMMMYYYTTTTTLLLLLLLLLLLLGGGGGGGGGGGGNNNVVVVVVVVVVVVVVVVVLLLLWLLLLLLGGGGGGGGGGGLLKKKKKKKKYZZZZZZZZZZZBJJJJJTTTTTTTTTTTTRSSTSTFFFFFX
DDDDDDDYMHMMYMYYTTTTTTLLLLLLLLLLLLGGGGGGGGGGGGNNNVVVVVVLLLZZLLLLLLLLLLWWWLLLLLGGGGGGGGGLLKKKKKKKKKYYZZZZZZZZZZBJJJJTTTTTTTTTTTTTTSSSSSSFFFFD
DDDYYYYYMMMYYYYYYTTTTTTLLCCCLLLLLLGGGGGGGGGGGGNNNVVVVVVLLLLLLLLLLLWLLLWWOLLLLLGGGGGGGGGLLKKKKKKKKKYYYMZZZZZZZVJJJJJTTTTTTTTTTTTTTSSSSSSSSSBD
DDYYYYYYMYYYYYMMYTTMTTTTTCCICCLLLLGGGGGGGGGGGGNNNVVVVVVLLLLLLLLLLLWLWLWWOLWLLLGGGGGGGGLLLLMRKKKKKYYYYYZZZZZZZZJJJJJTTJTTTTTTTTTTTOSSSSNNSDDD
DDDYYYYYYYYYYYMMMMTMTTTTCCCCCCCCVVVVVVVVGGGGGGNNNVVVVVVLLLLLLLLLWWWWWWWWOLWWGGGGGGGGGGGGGMMRXKKYYYYYYYYYZZZZZZJJJJJJJTTTTTTTTTTOOOSSSSNDDDDD
DDDYYYYYYYYYYYMMMMMMMTTTCCCCCCCCVVVVVVVYYOOOONNNNNNNNNNLLLLLLLLLWWWWWWWWWWWWGGGGGGGGGGGGMMMRXXXXXXYYYYYZZZZJJJJJJJJJJJTTTTTTOTOOOOOOSSDDDDDD
DDYYYYYYYYYYYYMMMMTTTTTCCCCCCCCCCCCVVVVYYOOONNNNNNNNNNNLLLLLLLLLIWWWWWWWWWWWGGGGGGGGGGGMMMMRXXXXXYYYYYYYYZZZBJJBJJJJJJJTTTTTOOOOOOOOOODDDDDD
DDDYYYYYYYYYYYMMMMMTTTTTTCCCCCCCCCCVVVVVYOONNNUNNNNNNLLLLLLLLLLLIWWWWWWWWWWWNGGGGGGGGGGGMCCXXXXXYYXYYYYYYYZBBJJBBJJJJJJJJQQTOOOOOOOODODDDDDD
DDDDDFYYYYYYYYYYYYMTTTTTTTTCCCCCCCVVVVVVYYYRRNUUUUNNNNBLLBLLLLLLLWWWWWWWWWVVVGGGGGGGGGGGCCCGXXXXXXXXYYYYYYZBBJJBJJJJJJJZLLQTTOOXOOOODDDDDDDD
XDXXDFYYYYYYYYYYIYYTTTTTTTTCCCCCCMVVVVVVYYYRRNRUNNNNNNBBLBLLLLNNNNWWWWWWWWVVVVGGGGGGGGCCCCCGGGCXXXXXXYYYYYBBBBBBJJJJJLLLLLQQQXXXXOOOODDDDDDD
XXXXFFFYYYYYYYYYYTTTTTTTTTCCCCCCCMCVVVVVVVRRRRNUNNNNNNBBBBLLLLNNNNWWWWWWVVVVVVGGGGGGGGGGCCCGCGCXXXXXXYYYYYBXBBBBBJJWLLLLLLLLXXXXOOOODDDDDDDD
XXQFFFFFYYYYYYYYFTTTTTTTTTCCCCCCCCCCVVVVVVRRRRNNNNNNNNRRRRLLLLUUUWWWUUUWWVVVVVVGGCCCGGCCCCCCCCCXXXXXVYYYYYYXXBBBBBLLLLLLLLLLXXXKKKKODDDDDDDD
XXXXXFFFYYBYYYYYYTTTTTTTTCCCCCCCCCCCLLVVMRRRRKQNNQNNQNRRRRLLDUUUTUUUUUUUVVVVVVVGCCCCCCCCCCCCCCCXXCCCVYYYYYYXXBBBLLLLLLLLLLLLLXXKKKOOODDDDDDD
XXXXXFFFFYBYBBBNYTTTTTTTTCCWWWCCCCCCLLLORRRBBQQQQQQQQQQQRRLLDDUUUUUUUUUUUUVVVCCCCCCCCCCCCCCCCCCCCCCVVYLYYYXXXXBBBBLLLLLLLLLLUXXKKKOOODDDDDDD
XXXFXFFFFBBBBBBNTTGGGTTTTCWWWWCJJJCLLLLLRRLBLLQQQQQQQQQQRRRLDDUUUUUUUUUUUUVUVVVCCCCCCCCCCCCCCCCCCCCCCCLYLYXXXXXBLLLLLLLLQHHHHXXKKKKKDDDDDDDD
XXXFFFFFFFBBBBBBYTGGGTTTTTZZWWWJJJCTLLLQQLLLLLLQQQQQQQRRRRRRDDUUUUUUUUUUUUUUVVVVVCCCWLWWCCCCCCCCCCCCCCLYLXXXXXBBLLLLLLLLLHHHHXHEKKKKKDYYKDDD
XXFFFFFFFFBBBBBBBTGGGTTTZTZZZZWWJJLLLLLLLLLLLLLQQQQQQQQRRRRRUUUUUUUUUUUUUUVVVVVVCCCNWWWWWWCCCCCCCCCCCCLLLXXXXXXBBBLLLLLLLHHHHHHKKKKKKNKKKDDD
XXFFFFFFFFBIBBBBBIITTTTTZZZZZZPWLLLLLLLLLLLLLLLQQQQQQRRRRRRRRUUUUUUUUUUUUUUVVVVVCVWNNWWWWCCCCWCCECCCRMRRLXXXXXXBBLLLLLLLLHHHHHOOKKKKKKKOODDD
XFFFFFFFFFBBBBBBBIIIITFTTEZZZZPLGLLLLLLLLLLLLLLLLQQBBRRRRRRRRRRUUOUUUUUUUHVVVVVVVVWWWWWWWCWWCWCCCCRRRRRLLXXXXXXBBLLLLLLLHHHHHHHKKKKKKKOOODYY
FFFFFFFFFFBBBBBBBBIIILZZZZZZZZLLLLLLLLLLLLLZZZLBBBBRRRRGGGGRBBBUPOUUUUUUHHVVVVVVVVWWWWWWWWWWWWWCCRRRRRRRRXXXXXXBBBLLLLLLHHHHHHHLKKKVKVTTTTYT
FFFFFFFFFFBBBBBBBIIIIZZRZZZZZZZLLLLLLLLLLLLZZZBBBBBRRRRGGGGGGGGPPOUUUUUUUVVVVVVVVVWWWWWWWWWWWRRRRRRRRRRRRXXXXXXBBBBLLLBLHHHHHHHHHVVVKVTTTTTT
FFFFFFFFFFBBBBBBBBIIZZZZZZZZZZZLLLLLLLLLLLLLLLBBBBRRRRRGGGGGGGGPPPUUUDDUUVVVBVWBVVVWWWWWWWWWWWRRWRRRRRERQQXXXXXBBBBLLLLLHJHHHHHHHVVVVVVVTTTT
FFFFFFFFFFBBBBBBIIIIIJZZZZZZZZZLLLLLLLLLLLLLBBBBBBRRRRRGGGGGGGPPPPUUDDDUBVBBBVWBVVVWWWWWWWWWWWWWWRRRRREEJJXXXJBBBBBBLLJJJJHHHHHVVVVVVVEVTTTT
FFFFFFFFFSBBBBBBIIIIIZZZZZZZZZZLLLQLLLLLULBBBBBBBRRRRRRGHGGHHGPPPPPPPPPBBBBBBBBBFWWWWWWWWWWWWWWWWRRRRRRJJJJXXJJBBBBBJJJJJJHJHVVVVVVVVVETTTTT
FFFFFFFFFSFIIIBBIIIIZZZZZZZMZZZZLLLHLLLLLBBBBBBBBRRRRRRRHHHHHHHHHPPPPPBBBBBBBBBBBPWWWWWWWWWWWWWWWRRRRZZZJJXXXJJJBBBBJJJJJJHJJJVVVVVVVVEETTTT
FFFFFFFFFFFIIIBIIIIIIIIIZZMMZZZLLLLLLLEELBBBBBBBBRRRRRRRRHHHHHHHHPPPPBBBBBBBBBBBBWWWWWWWWWWWWWWWWRRYRRRJJJJJJJBBBBBBJJJJJJJJJJVVVZZZZEEEEETI
FFFFFFFFFFFFFIIIIIIIIIIPZHHMMZLLLLLLLLLLLBBBBBBBBBBRRRRRHHHHHHHHPPPPPPBBBBBBBBBBBWWWWWWWWWWWWWWWWRRRJRJJJJJJJJBBBBBJJJJJJJJJJJVVVZZZZEEEEEII
FFFFFFFFFFFIIIIIIIIIIIIPPFHFMZLLLLLLLLLLLLBBBBBBBHHHUURHHHHHHHHPPPPPPPBBBBBBBBBBBWWBGGWWWWWWWWWWJJRJJJJJJJJJJBBBBBBJJJJJJJJJJJVVVVZZZZEEEYYY
EFTTFFFFFFIIIIIIIIIIIIIIIFFFMLLLLLLLLLLLLLBBBBBBBBBHHHHHHHHHHHHHHPPBBBBBBBBBBBBBBBBBGSSRSWWWWWWWJJJJJJJJJJRJJBBBBBBBJJJJJJJJJJJVVVZZZGEYYYYY
EEFFFFFFFFFFFIIIIIIIIIFFFFFHFLLLLLLLLLLLLLBBBBBBBBBHHHHHHHHHHHHHHHHBBBBBBBBBBBBBBBBBSSSSSSWWWWWWJJJJJJJJJJJJBBBBBBBBBIIRJJJJJJJWVVVVYYXXYYYY
EEFFFFFFFFFFFIIIIIIIIFFFFFFFFFLLLLLLLLLLLLBBBDBBBBBBHHHHHHHHHHHHHHHBBBBBBBBBBBSBBBBBBBSSSSSWWWWWWJJJJJJJJJBBBBBBBIBBIIIIIIJWWWWWWWWVYYYYYYYY
EEFFFFFFFFFFIIFIIIIIWFFFFFFFFLLLLLLLLLLLLLLDDDBDBBDBHHHHHHHHHHHHHHHBBBBBBBBBBBSBBBBBBBSSSSSSSWWWWJJJJJJJJJJJBBBIIIBBIIIIIIIOWWWWWWYYYYYYYYYY
EFFFFFFFFFFFFFFIIIIIWTFFFFFFFLLLLLLLLLLLLYYYYDDDDDDBHHHHHHHHHHHHHHHHHBBBBBBBBBBBBBBBBBSSWSSSSWWWWWWJJJZJXXXJBBBIIIIIIIIIIIZWWWWWWYYYYYYYYYYY
EEZFFFFFFFFFFFFIIIIWWTFFFFFFFLLLLLLLYYYYYYYYYDDDDGHHHHHAHHHHHHHHHHHHLBBBBBYBBBYBBBBBBBWWWSSSSWWWWEEJEJJXXXQRBBBIIIIIIIIIIIIIWWWWWWYYYYYYYYYY
ZZZBFFFFFFFFFFFIIIIIWFFFFFFFFLLLLLLLLYYYYYYYDDDDDDPPHHAAAAHHHHHHHHHHLBYBYYYYYYYYBBBBBWWWWSSWSWWWWEEEEQQQQQQRBBFNNRIIIIIIIIIICWWWWYYYYYYYYYYY
ZZZBFFFFFFFFFIIIIGIIWWFFFFFCFULLLLLMLYYYYYYYYDDDDDEPPPAAAAAHHHHHHHHHHHYYYYYYYYYBBBBBBWWWWWSWSWWEEEEEEQQQQQQRRBBNNRRIIIIIIIICCWWWWWYYYYYYYYYY
ZZZZZZFFFFFFFIIIIIIWWFFFFFFFWWLLLLLLYYYYYYYYYYEDEEEPAAAAAAKKKHHHHHHHHHHHYYYYYYBBBBBBBZWWWWWWWWWEEEEEEQQQQQQQRBBNNNIIIIIIICCCCCTTWTYYYYYYYYYY
ZZZZZZZFFFUFFIIIIIIWWWWFFFFFWWWLLLYLYRYYYYYYYYEEEEEAAAAAAAKHHHHHHHHHHHHHYYYYYYYBBBBBBZWWWWWWWWEEEEEEEQQQQQQRRQQQNNIIIIIIICCCCTTTTTTYYYYYYYYY
ZZZZZFFFFFFFFFFIIIIIWWWWFFWWWWWLWWYYYRYRYYYYYYEEEEEAAAAAAAAHHLLWHHHHHHHHHYYYYYYYBBBBBZWWWWWWWWWWEEVEEQQQQQQQQQQQNNNIICCCCCCCCTTTTTTTYYYYYYYY
ZZZZZVVVVFFFFVVIIIIWWWWWFWWWWWWWWWWRRRRRRYYYYYEEEEEEAAAAAALLLLLWHHHHHHHYYYYYYYYYYZZZZZZZZWWWWWWWWEVVQNQQQQQQQQQNNNNNICCCCCCCCCCTTTTTTTYYYYYY
ZZZZZZVVVVVFFVVIWWWWWWWWWWWWWWWWRRRRRRRRRRIYIIIEEEEEAEEEALLLLLLHHHHHHHYYYYYYYYZZZZEZZZZZZWWWVWWWVWVQQQQQQQQQQQQQNNNNCCCCCCCCCTTTTTTTTTYYYYYY
VVVZZVVVVVVVVVVVVWWVWWWWWWWWWWWWRRRRRRRRRRIYIIIEEEEEEEEEEELLLLLHHHHDHYYYYYZZZZZZZZZZZZZZWWWVVVVWVVVVVQBQQQQQQQNNNNNNNNCCCCCCCTTTTTTTTTYYYYYY
VVVZVVVVVVVVVVVVVVVVWWWWWWWWWWWWRRRRRRRRRRIIIEEEEEEEEEEEELLLLLLLLLLLYYYYRYZZZZZZZZZZZZZZWWWWVVVVVVVVQQQQQQQQQNNNNNNNNCCCCCCCCTTTTTTTTYYYYYYY
VVVVVVVVVVVVVVVVVVVVWWWWWWWWWWWWBRRRRRRRIIIIIEEEEEEEEEEEEELLLLLLLLLYYYYRRRKKZZZZZZZZZZZZZZVVVVVVVVVVVQQQQQQQNNNNNNNNNCCCCCCCCTTTTTTTTTTYYYYY
VVVVVVVVVVVVVVVVVVVVVWWWWWWWWWWWRRRRRRRRIIIIIEEEEEEEEEEEELLLLLLLLLLLYYYRRRRKZZZZZZZZZZZZZZVVVVVVVVVVVVQQNQQNNNNNNNNNNCCCCCCCCTTTTTTTTTYYYYYY
VVVVVVVVVVVVVVVVVVVVWWWWWWWWWWWWWRRRRRRRRIEEEEEEEEEEEEEELLLLLLLLLLLLLYRRRRRZZZZZZZZZZZZZZZWVVVVVVVVVVVNNNNNNNNNNNNNNNNNCCCCTTTTTTTTTTTYYYYYT
VVVVVVVVVVVVVVVVVWWWWWWWWWWWWWWWWRRRRRRRRREEEEEEEEEEEEEEELLLLLLLLLLEERRRRRZZZZZZZZZZZZZZZZZVVVVVVVVVVVNNNNNNNNNNNNNNNCCCCDCTTTTTTTTTTTYYYYTT")
