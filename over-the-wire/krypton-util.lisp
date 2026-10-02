(in-package #:krypton)

;;; ---- raw ciphertext samples ------------------------------------------------

(defparameter *found1*
  "CGZNL YJBEN QYDLQ ZQSUQ NZCYD SNQVU BFGBK GQUQZ QSUQN UZCYD SNJDS UDCXJ ZCYDS NZQSU QNUZB WSBNZ QSUQN UDCXJ CUBGS BXJDS UCTYV SUJQG WTBUJ KCWSV LFGBK GSGZN LYJCB GJSZD GCHMS UCJCU QJLYS BXUMA UJCJM JCBGZ CYDSN CGKDC ZDSQZ DVSJJ SNCGJ DSYVQ CGJSO JCUNS YVQZS WALQV SJJSN UBTSX COSWG MTASN BXYBU CJCBG UWBKG JDSQV YDQAS JXBNS OQTYV SKCJD QUDCX JBXQK BMVWA SNSYV QZSWA LWAKB MVWAS ZBTSS QGWUB BGJDS TSJDB WCUGQ TSWQX JSNRM VCMUZ QSUQN KDBMU SWCJJ BZBTT MGCZQ JSKCJ DDCUE SGSNQ VUJDS SGZNL YJCBG UJSYY SNXBN TSWAL QZQSU QNZCY DSNCU BXJSG CGZBN YBNQJ SWQUY QNJBX TBNSZ BTYVS OUZDS TSUUM ZDQUJ DSICE SGNSZ CYDSN QGWUJ CVVDQ UTBWS NGQYY VCZQJ CBGCG JDSNB JULUJ STQUK CJDQV VUCGE VSQVY DQASJ UMAUJ CJMJC BGZCY DSNUJ DSZQS UQNZC YDSNC USQUC VLANB FSGQG WCGYN QZJCZ SBXXS NUSUU SGJCQ VVLGB ZBTTM GCZQJ CBGUS ZMNCJ LUDQF SUYSQ NSYNB WMZSW TBUJB XDCUF GBKGK BNFAS JKSSG QGWDC USQNV LYVQL UKSNS TQCGV LZBTS WCSUQ GWDCU JBNCS UESGN SUDSN QCUSW JBJDS YSQFB XUBYD CUJCZ QJCBG QGWQN JCUJN LALJD SSGWB XJDSU COJSS GJDZS GJMNL GSOJD SKNBJ STQCG VLJNQ ESWCS UMGJC VQABM JCGZV MWCGE DQTVS JFCGE VSQNQ GWTQZ ASJDZ BGUCW SNSWU BTSBX JDSXC GSUJS OQTYV SUCGJ DSSGE VCUDV QGEMQ ESCGD CUVQU JYDQU SDSKN BJSJN QECZB TSWCS UQVUB FGBKG QUNBT QGZSU QGWZB VVQAB NQJSW KCJDB JDSNY VQLKN CEDJU TQGLB XDCUY VQLUK SNSYM AVCUD SWCGS WCJCB GUBXI QNLCG EHMQV CJLQG WQZZM NQZLW MNCGE DCUVC XSJCT SQGWC GJKBB XDCUX BNTSN JDSQJ NCZQV ZBVVS QEMSU YMAVC UDSWJ DSXCN UJXBV CBQZB VVSZJ SWSWC JCBGB XDCUW NQTQJ CZKBN FUJDQ JCGZV MWSWQ VVAMJ JKBBX JDSYV QLUGB KNSZB EGCUS WQUUD QFSUY SQNSU")

(defparameter *found2*
  "QVJDB MEDGB QJJSG WQGZS NSZBN WUXBN JDSYS NCBWU MNICI STBUJ ACBEN QYDSN UQENS SJDQJ UDQFS UYSQN SKQUS WMZQJ SWQJJ DSFCG EUGSK UZDBB VCGUJ NQJXB NWQXN SSUZD BBVZD QNJSN SWCGQ ABMJQ HMQNJ SNBXQ TCVSX NBTDC UDBTS ENQTT QNUZD BBVUI QNCSW CGHMQ VCJLW MNCGE JDSSV CPQAS JDQGS NQAMJ JDSZM NNCZM VMTKQ UWCZJ QJSWA LVQKJ DNBME DBMJS GEVQG WQGWJ DSUZD BBVKB MVWDQ ISYNB ICWSW QGCGJ SGUCI SSWMZ QJCBG CGVQJ CGENQ TTQNQ GWJDS ZVQUU CZUQJ JDSQE SBXUD QFSUY SQNST QNNCS WJDSL SQNBV WQGGS DQJDQ KQLJD SZBGU CUJBN LZBMN JBXJD SWCBZ SUSBX KBNZS UJSNC UUMSW QTQNN CQESV CZSGZ SBGGB ISTAS NJKBB XDQJD QKQLU GSCED ABMNU YBUJS WABGW UJDSG SOJWQ LQUUM NSJLJ DQJJD SNSKS NSGBC TYSWC TSGJU JBJDS TQNNC QESJD SZBMY VSTQL DQISQ NNQGE SWJDS ZSNST BGLCG UBTSD QUJSU CGZSJ DSKBN ZSUJS NZDQG ZSVVB NQVVB KSWJD STQNN CQESA QGGUJ BASNS QWBGZ SCGUJ SQWBX JDSMU MQVJD NSSJC TSUQG GSUYN SEGQG ZLZBM VWDQI SASSG JDSNS QUBGX BNJDC UUCOT BGJDU QXJSN JDSTQ NNCQE SUDSE QISAC NJDJB QWQME DJSNU MUQGG QKDBK QUAQY JCUSW BGTQL JKCGU UBGDQ TGSJQ GWWQM EDJSN RMWCJ DXBVV BKSWQ VTBUJ JKBLS QNUVQ JSNQG WKSNS AQYJC USWBG XSANM QNLDQ TGSJW CSWBX MGFGB KGZQM USUQJ JDSQE SBXQG WKQUA MNCSW BGQME MUJQX JSNJD SACNJ DBXJD SJKCG UJDSN SQNSX SKDCU JBNCZ QVJNQ ZSUBX UDQFS UYSQN SMGJC VDSCU TSGJC BGSWQ UYQNJ BXJDS VBGWB GJDSQ JNSUZ SGSCG ASZQM USBXJ DCUEQ YUZDB VQNUN SXSNJ BJDSL SQNUA SJKSS GQGWQ UUDQF SUYSQ NSUVB UJLSQ NUACB ENQYD SNUQJ JSTYJ CGEJB QZZBM GJXBN JDCUY SNCBW DQISN SYBNJ SWTQG LQYBZ NLYDQ VUJBN CSUGC ZDBVQ UNBKS UDQFS UYSQN SUXCN UJACB ENQYD SNNSZ BMGJS WQUJN QJXBN WVSES GWJDQ JUDQF SUYSQ NSXVS WJDSJ BKGXB NVBGW BGJBS UZQYS YNBUS ZMJCB GXBNW SSNYB QZDCG EQGBJ DSNSC EDJSS GJDZS GJMNL UJBNL DQUUD QFSUY SQNSU JQNJC GEDCU JDSQJ NCZQV ZQNSS NTCGW CGEJD SDBNU SUBXJ DSQJN SYQJN BGUCG VBGWB GRBDG QMANS LNSYB NJSWJ DQJUD QFSUY SQNSD QWASS GQZBM GJNLU ZDBBV TQUJS NUBTS JKSGJ CSJDZ SGJMN LUZDB VQNUD QISUM EESUJ SWJDQ JUDQF SUYSQ NSTQL DQISA SSGST YVBLS WQUQU ZDBBV TQUJS NALQV SOQGW SNDBE DJBGB XVQGZ QUDCN SQZQJ DBVCZ VQGWB KGSNK DBGQT SWQZS NJQCG KCVVC QTUDQ FSUDQ XJSCG DCUKC VVGBS ICWSG ZSUMA UJQGJ CQJSU UMZDU JBNCS UBJDS NJDQG DSQNU QLZBV VSZJS WQXJS NDCUW SQJD")

(defparameter *found3*
  "DSNSM YBGVS ENQGW QNBUS KCJDQ ENQIS QGWUJ QJSVL QCNQG WANBM EDJTS JDSAS SJVSX NBTQE VQUUZ QUSCG KDCZD CJKQU SGZVB USWCJ KQUQA SQMJC XMVUZ QNQAQ SMUQG WQJJD QJJCT SMGFG BKGJB GQJMN QVCUJ UBXZB MNUSQ ENSQJ YNCPS CGQUZ CSGJC XCZYB CGJBX ICSKJ DSNSK SNSJK BNBMG WAVQZ FUYBJ UGSQN BGSSO JNSTC JLBXJ DSAQZ FQGWQ VBGEB GSGSQ NJDSB JDSNJ DSUZQ VSUKS NSSOZ SSWCG EVLDQ NWQGW EVBUU LKCJD QVVJD SQYYS QNQGZ SBXAM NGCUD SWEBV WJDSK SCEDJ BXJDS CGUSZ JKQUI SNLNS TQNFQ AVSQG WJQFC GEQVV JDCGE UCGJB ZBGUC WSNQJ CBGCZ BMVWD QNWVL AVQTS RMYCJ SNXBN DCUBY CGCBG NSUYS ZJCGE CJ")

(defparameter *krypton4*
  "KSVVW BGSJD SVSIS VXBMN YQUUK BNWCU ANMJS")

;;; ---- utilities -------------------------------------------------------------

(defun clean (string)
  "Return STRING uppercased with all non-alpha characters removed."
  (string-upcase (remove-if-not #'alpha-char-p string)))

(defun combined-corpus ()
  "Concatenate and clean all found ciphertext samples."
  (concatenate 'string
               (clean *found1*)
               (clean *found2*)
               (clean *found3*)))

;;; ---- frequency analysis ----------------------------------------------------

(defun hash-table-to-sorted-alist (ht)
  (sort (a:hash-table-alist ht) #'> :key #'cdr))

(defun ascii-histogram (text &key uppercase-only)
  (loop
    :with h := (make-hash-table)
    :for c :across (if uppercase-only (string-upcase text) text)
    :when (uiop/cl:alpha-char-p c)
      :do (incf (gethash c h 0))
    :finally (return h)))

(defun detect-shift (src dst)
  (mod (- (char-code dst) (char-code src)) 26))

(defparameter *english-freq*
  '((#\E . nil)
    (#\T . nil)
    (#\A . nil)
    (#\O . nil)
    (#\I . nil)
    (#\N . nil)
    (#\S . nil)
    (#\R . nil)
    (#\H . nil)
    (#\D . nil)
    (#\L . nil)
    (#\U . nil)
    (#\C . nil)
    (#\M . nil)
    (#\F . nil)
    (#\Y . nil)
    (#\W . nil)
    (#\G . nil)
    (#\P . nil)
    (#\B . nil)
    (#\V . nil)
    (#\K . nil)
    (#\X . nil)
    (#\Q . nil)
    (#\J . nil)
    (#\Z . nil)))

(defparameter *english-frequencies*
  '((#\A (:text . 8.2) (:dict . 7.8))	
    (#\B (:text . 1.5) (:dict . 2.0))	
    (#\C (:text . 2.8) (:dict . 4.0))	
    (#\D (:text . 4.3) (:dict . 3.8))	
    (#\E (:text . 12.7) (:dict . 11.0))	
    (#\F (:text . 2.2) (:dict . 1.4))	
    (#\G (:text . 2.0) (:dict . 3.0))	
    (#\H (:text . 6.1) (:dict . 2.3))	
    (#\I (:text . 7.0) (:dict . 8.6))	
    (#\J (:text . 0.16) (:dict . 0.25))	
    (#\K (:text . 0.77) (:dict . 0.97))	
    (#\L (:text . 4.0) (:dict . 5.3))	
    (#\M (:text . 2.4) (:dict . 2.7))	
    (#\N (:text . 6.7) (:dict . 7.2))	
    (#\O (:text . 7.5) (:dict . 6.1))	
    (#\P (:text . 1.9) (:dict . 2.8))	
    (#\Q (:text . 0.12) (:dict . 0.19))	
    (#\R (:text . 6.0) (:dict . 7.3))	
    (#\S (:text . 6.3) (:dict . 8.7))	
    (#\T (:text . 9.1) (:dict . 6.7))	
    (#\U (:text . 2.8) (:dict . 3.3))	
    (#\V (:text . 0.98) (:dict . 1.0))	
    (#\W (:text . 2.4) (:dict . 0.91))	
    (#\X (:text . 0.15) (:dict . 0.27))	
    (#\Y (:text . 2.0) (:dict . 1.6))	
    (#\Z (:text . 0.074) (:dict . 0.44))))

(defun extract-frequency-map (raw-data key)
  (assert (member key '(:text :dict)))
  (loop
    :for letter :in raw-data
    :collect (cons (car letter) (cdr (assoc key (cdr letter)))) :into m
    :finally (return (sort m #'> :key #'cdr))))

(defun zip-alist (src-freq dst-freq)
  (loop
    :for (src . nil) :in src-freq
    :for (dst . nil) :in dst-freq
    :collect (list src dst)))

(defun make-translator (src-freq dst-freq &key decode)
  (loop
    :with ht := (make-hash-table)
    :for (k v) :in (if decode
                       (zip-alist dst-freq src-freq)
                       (zip-alist src-freq dst-freq))
    :do (setf (gethash k ht) v)
    :finally (return ht)))

(defun make-translator-from-str (src-freq dst-freq)
  (loop
    :with ht := (make-hash-table)
    :for s :across src-freq
    :for d :across dst-freq
    :do (setf (gethash s ht) d)
    :finally (return ht)))

(defun translate (text translator)
  (loop
    :for c :across text
    :collect (gethash c translator c) :into target
    :finally (return (coerce target 'string))))

;; krypton4
;; "GECCLIHEARECEKECFIUNMTOOGINLSOBNUAE"

;;; ---- Caesar cipher ---------------------------------------------------------

(defun rotate (string shift)
  "Rotate every letter in STRING by SHIFT positions (Caesar cipher)."
  (flet ((rotate-char (c)
           (cond ((upper-case-p c)
                  (let ((a (char-code #\A)))
                    (code-char (+ (mod (+ (- (char-code c) a) shift) 26) a))))
                 ((lower-case-p c)
                  (let ((a (char-code #\a)))
                    (code-char (+ (mod (+ (- (char-code c) a) shift) 26) a))))
                 (t c))))
    (map 'string #'rotate-char string)))

(defun find-caesar-shift (ciphertext)
  "Return all 26 (shift chi-sq) pairs sorted by chi-sq ascending (best first).
The top hit is the most likely Caesar shift."
  (let ((n (count-if #'alpha-char-p ciphertext)))
    (sort
     (loop for shift from 0 to 25
           for candidate = (rotate ciphertext (- shift))
           for hist = (histogram (string-downcase candidate))
           collect (cons shift (chi-squared hist n)))
     #'< :key #'cdr)))

(defun crack-caesar (ciphertext)
  "Return the most likely plaintext and the shift used."
  (let* ((best-shift (car (first (find-caesar-shift ciphertext))))
         (plaintext  (rotate ciphertext (- best-shift))))
    (values plaintext best-shift)))

;;; ---- Vigenère cipher -------------------------------------------------------

(defun index-of-coincidence (text)
  "Index of Coincidence for TEXT (English ~0.065, random ~0.038)."
  (let* ((n (count-if #'alpha-char-p text))
         (hist (histogram (string-downcase text)))
         (sum (loop for (nil . count) in hist sum (* count (1- count)))))
    (if (> n 1) (/ sum (* n (1- n))) 0)))

(defun nth-chars (text n offset)
  "Extract every Nth character from TEXT starting at OFFSET."
  (loop for i from offset below (length text) by n
        collect (char text i) into chars
        finally (return (coerce chars 'string))))

(defun score-key-length (text klen)
  "Average IoC across all KLEN substrings of TEXT."
  (coerce (/ (loop for i from 0 below klen
            sum (index-of-coincidence (nth-chars text klen i)))
      klen) 'double-float))

(defun find-key-length (text &key (max-klen 12))
  "Return plausible key lengths sorted by average IoC descending.
Values near 0.065 indicate correct English-like subgroups."
  (sort (loop for klen from 1 to max-klen
              collect (cons klen (score-key-length text klen)))
        #'> :key #'cdr))

(defun crack-vigenere (ciphertext key-length)
  "Given CIPHERTEXT and KEY-LENGTH, return (values plaintext key-string)."
  (let* ((key (coerce
               (loop for i from 0 below key-length
                     for group = (nth-chars ciphertext key-length i)
                     for shift = (car (first (find-caesar-shift group)))
                     collect (code-char (+ (char-code #\A) shift)))
               'string))
         (plaintext (loop for i from 0 below (length ciphertext)
                          for k = (- (char-code (char key (mod i key-length)))
                                     (char-code #\A))
                          collect (char (rotate (string (char ciphertext i)) (- k)) 0))))
    (values (coerce plaintext 'string) key)))
