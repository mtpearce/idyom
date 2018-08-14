(cl:in-package #:ppm)

(5am:def-suite ppm :in testing::idyom-tests)
(5am:in-suite ppm)

(defun test-ppm (sequence &key (exclusion t) (escape :c) (mixtures t) (update-exclusion nil) (order-bound nil) ps write)
  (let* ((alphabet (sort (remove-duplicates sequence) #'string<=))
         (model (ppm:make-ppm alphabet :exclusion exclusion :escape escape :mixtures mixtures
                              :update-exclusion update-exclusion :order-bound order-bound))
         (result (ppm:model-dataset model (list sequence) :construct? t :predict? t)))
    (prog1 result
      (when write (write result :right-margin 110))
      (when ps (ppm:write-model-to-postscript model ps)))))

;; Simple Tests
;; ===========================================================================

(5am:def-suite ppm*-simple :in ppm)
(5am:in-suite ppm*-simple)

(5am:test kkkkkkkkk-ppmc*
  (5am:is (equal (test-ppm '(k k k k k k k k k) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0)))
                    (K ((K 1.0))))))))

(5am:test aaaaaaaabb-ppmc*
  (5am:is (equal (test-ppm '(a a a a a a a a b b) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.5) (B 0.5)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (A ((A 0.6666667) (B 0.33333334)))
                    (B ((A 0.6666667) (B 0.33333334)))
                    (B ((A 0.88888884) (B 0.111111104))))))))
                 
(5am:test ababc-ppmc*
  (5am:is (equal (test-ppm '(a b a b c) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.33333334) (B 0.33333334) (C 0.33333334)))
                    (B ((A 0.59999996) (B 0.19999999) (C 0.19999999)))
                    (A ((A 0.33333334) (B 0.33333334) (C 0.33333334)))
                    (B ((A 0.2857143) (B 0.5714286) (C 0.14285715)))
                    (C ((A 0.5714286) (B 0.2857143) (C 0.14285715))))))))


;; PPM* without exclusion
;; ===========================================================================

(5am:def-suite ppm*-exclusion :in ppm)
(5am:in-suite ppm*-exclusion)

(5am:test abracadabra-ppmc*-without-exclusion
  (5am:is (equal (test-ppm '(a b r a c a d a b r a) :exclusion nil :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.2) (B 0.2) (C 0.2) (D 0.2) (R 0.2)))
                    (B ((A 0.5555555) (B 0.111111104) (C 0.111111104) (D 0.111111104) (R 0.111111104)))
                    (R ((A 0.31249997) (B 0.31249997) (C 0.12499999) (D 0.12499999) (R 0.12499999)))
                    (A ((A 0.23809522) (B 0.23809522) (C 0.14285713) (D 0.14285713) (R 0.23809522)))
                    (C ((A 0.17857143) (B 0.625) (C 0.053571425) (D 0.053571425) (R 0.08928572)))
                    (A ((A 0.3448276) (B 0.1724138) (C 0.1724138) (D 0.13793102) (R 0.1724138)))
                    (D ((A 0.20270272) (B 0.33783785) (C 0.33783785) (D 0.054054055) (R 0.06756757)))
                    (A ((A 0.42857143) (B 0.14285715) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (B ((A 0.22222221) (B 0.24074072) (C 0.24074072) (D 0.24074072) (R 0.055555552)))
                    (R ((A 0.18181819) (B 0.09090909) (C 0.045454547) (D 0.045454547) (R 0.6363636)))
                    (A ((A 0.7142857) (B 0.0952381) (C 0.04761905) (D 0.04761905) (R 0.0952381))))))))


;; PPM* with Blending
;; ===========================================================================

(5am:def-suite ppm*-blending :in ppm)
(5am:in-suite ppm*-blending)

;; abracadabra (Cleary & Teahan, 1997; Bunton, 1996)
(5am:test abracadabra-ppmc*
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.5555555) (B 0.111111104) (C 0.111111104) (D 0.111111104) (R 0.111111104)))
                    (R ((A 0.2857143) (B 0.2857143) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (C ((A 0.18181819) (B 0.54545456) (C 0.09090909) (D 0.09090909) (R 0.09090909)))
                    (A ((A 0.2857143) (B 0.14285715) (C 0.14285715) (D 0.28571427) (R 0.14285715)))
                    (D ((A 0.21428573) (B 0.2857143) (C 0.2857143) (D 0.14285715) (R 0.071428575)))
                    (A ((A 0.42857143) (B 0.14285715) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (B ((A 0.26666665) (B 0.22222221) (C 0.22222221) (D 0.22222221) (R 0.06666666)))
                    (R ((A 0.19047621) (B 0.095238104) (C 0.047619052) (D 0.047619052) (R 0.61904764)))
                    (A ((A 0.6470587) (B 0.117647044) (C 0.058823522) (D 0.058823522) (R 0.117647044)))
                    (C ((A 0.11627906) (B 0.18604651) (C 0.5581395) (D 0.093023255) (R 0.046511628))))))))

;; letlettertele (Larsson, 1999)
(5am:test letlettertele-ppmc*
  (5am:is (equal (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (L ((E 0.25) (L 0.25) (R 0.25) (T 0.25)))
                    (E ((E 0.14285715) (L 0.5714286) (R 0.14285715) (T 0.14285715)))
                    (T ((E 0.29999998) (L 0.29999998) (R 0.19999999) (T 0.19999999)))
                    (L ((E 0.22222221) (L 0.22222221) (R 0.3333333) (T 0.22222221)))
                    (E ((E 0.5714286) (L 0.19047621) (R 0.14285715) (T 0.095238104)))
                    (T ((E 0.16000001) (L 0.16000001) (R 0.11999999) (T 0.56)))
                    (T ((E 0.16000001) (L 0.56) (R 0.11999999) (T 0.16000001)))
                    (E ((E 0.2352941) (L 0.29411763) (R 0.17647058) (T 0.29411763)))
                    (R ((E 0.13333333) (L 0.08888889) (R 0.06666666) (T 0.7111112)))
                    (T ((E 0.33333334) (L 0.22222222) (R 0.11111111) (T 0.33333334)))
                    (E ((E 0.2777778) (L 0.2777778) (R 0.16666667) (T 0.2777778)))
                    (L ((E 0.11111113) (L 0.055555563) (R 0.5555556) (T 0.2777778)))
                    (E ((E 0.75) (L 0.09374999) (R 0.031249998) (T 0.12499999))))))))
                 
;; assanissimassa (Blelloch, 2001)
(5am:test assanissimassa-ppmc*
  (5am:is (equal (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.19999999) (I 0.19999999) (M 0.19999999) (N 0.19999999) (S 0.19999999)))
                    (S ((A 0.5555555) (I 0.111111104) (M 0.111111104) (N 0.111111104) (S 0.111111104)))
                    (S ((A 0.2857143) (I 0.14285715) (M 0.14285715) (N 0.14285715) (S 0.2857143)))
                    (A ((A 0.18181819) (I 0.090909086) (M 0.090909086) (N 0.090909086) (S 0.54545456)))
                    (N ((A 0.26666668) (I 0.06666667) (M 0.06666667) (N 0.06666667) (S 0.53333336)))
                    (I ((A 0.2857143) (I 0.14285715) (M 0.14285715) (N 0.14285715) (S 0.2857143)))
                    (S ((A 0.25) (I 0.125) (M 0.24999996) (N 0.125) (S 0.25)))
                    (S ((A 0.3) (I 0.10000001) (M 0.2) (N 0.10000001) (S 0.3)))
                    (I ((A 0.54545456) (I 0.045454547) (M 0.090909086) (N 0.045454547) (S 0.27272728)))
                    (M ((A 0.12500001) (I 0.12500001) (M 0.12499999) (N 0.06250001) (S 0.5625)))
                    (A ((A 0.20000002) (I 0.20000002) (M 0.10000001) (N 0.10000001) (S 0.40000004)))
                    (S ((A 0.1764706) (I 0.11764707) (M 0.058823533) (N 0.32352942) (S 0.32352942)))
                    (S ((A 0.12727273) (I 0.12727273) (M 0.05454546) (N 0.05454546) (S 0.6363636)))
                    (A ((A 0.5675675) (I 0.18918918) (M 0.027027026) (N 0.027027026) (S 0.18918917))))))))

;; mississippi (Witten et al., 1999)
(5am:test mississippi-ppmc*
  (5am:is (equal (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (M ((I 0.25) (M 0.25) (P 0.25) (S 0.25)))
                    (I ((I 0.14285715) (M 0.5714286) (P 0.14285715) (S 0.14285715)))
                    (S ((I 0.29999998) (M 0.29999998) (P 0.19999999) (S 0.19999999)))
                    (S ((I 0.22222221) (M 0.22222221) (P 0.3333333) (S 0.22222221)))
                    (I ((I 0.11764705) (M 0.11764705) (P 0.17647058) (S 0.58823526)))
                    (S ((I 0.19047621) (M 0.095238104) (P 0.14285715) (S 0.5714286)))
                    (S ((I 0.19047621) (M 0.0952381) (P 0.14285713) (S 0.5714286)))
                    (I ((I 0.55172414) (M 0.06896552) (P 0.10344827) (S 0.27586207)))
                    (P ((I 0.24000002) (M 0.080000006) (P 0.11999999) (S 0.56)))
                    (P ((I 0.33333334) (M 0.11111111) (P 0.11111111) (S 0.44444445)))
                    (I ((I 0.14999999) (M 0.049999997) (P 0.59999996) (S 0.19999999))))))))

;; woolloomooloo (Williams, 1991)
(5am:test woolloomooloo-ppmc*
  (5am:is (equal (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (W ((L 0.25) (M 0.25) (O 0.25) (W 0.25)))
                    (O ((L 0.14285715) (M 0.14285715) (O 0.14285715) (W 0.5714286)))
                    (O ((L 0.19999999) (M 0.19999999) (O 0.29999998) (W 0.29999998)))
                    (L ((L 0.12499999) (M 0.12499999) (O 0.5625) (W 0.1875)))
                    (L ((L 0.18181819) (M 0.27272725) (O 0.36363637) (W 0.18181819)))
                    (O ((L 0.5714286) (M 0.14285715) (O 0.19047621) (W 0.095238104)))
                    (O ((L 0.30769232) (M 0.23076925) (O 0.30769232) (W 0.15384616)))
                    (M ((L 0.55172414) (M 0.10344827) (O 0.27586207) (W 0.06896552)))
                    (O ((L 0.25) (M 0.125) (O 0.5) (W 0.125)))
                    (O ((L 0.21739131) (M 0.21739131) (O 0.43478262) (W 0.13043478)))
                    (L ((L 0.3125) (M 0.3125) (O 0.3125) (W 0.0625)))
                    (O ((L 0.64285713) (M 0.07142857) (O 0.21428572) (W 0.07142857)))
                    (O ((L 0.20833334) (M 0.10416667) (O 0.625) (W 0.0625))))))))
                 
;; agcgacgag (Giegerich & Kurtz, 1994, 1995)
(5am:test agcgacgag-ppmc*
  (5am:is (equal (test-ppm '(a g c g a c g a g) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.33333334) (C 0.33333334) (G 0.33333334)))
                    (G ((A 0.59999996) (C 0.19999999) (G 0.19999999)))
                    (C ((A 0.33333334) (C 0.33333334) (G 0.33333334)))
                    (G ((A 0.33333334) (C 0.33333334) (G 0.33333334)))
                    (A ((A 0.11111112) (C 0.6666667) (G 0.22222224)))
                    (C ((A 0.22222224) (C 0.11111112) (G 0.6666667)))
                    (G ((A 0.18181819) (C 0.18181819) (G 0.6363636)))
                    (A ((A 0.59999996) (C 0.19999999) (G 0.19999997)))
                    (G ((A 0.2) (C 0.6) (G 0.20000002))))))))


;; PPM* with Blending and Update Exclusion
;; ===========================================================================

(5am:def-suite ppm-blending-update-exclusion :in ppm)
(5am:in-suite ppm-blending-update-exclusion)

;; abracadabra (Cleary & Teahan, 1997; Bunton, 1996)
(5am:test abracadabra-ppmc*u
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion t :order-bound nil)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.5555555) (B 0.111111104) (C 0.111111104) (D 0.111111104) (R 0.111111104)))
                    (R ((A 0.2857143) (B 0.2857143) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (C ((A 0.18181819) (B 0.54545456) (C 0.09090909) (D 0.09090909) (R 0.09090909)))
                    (A ((A 0.2857143) (B 0.14285715) (C 0.14285715) (D 0.28571427) (R 0.14285715)))
                    (D ((A 0.21428573) (B 0.2857143) (C 0.2857143) (D 0.14285715) (R 0.071428575)))
                    (A ((A 0.42857143) (B 0.14285715) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (B ((A 0.26666665) (B 0.22222221) (C 0.22222221) (D 0.22222221) (R 0.06666666)))
                    (R ((A 0.21052633) (B 0.052631583) (C 0.052631583) (D 0.052631583) (R 0.631579)))
                    (A ((A 0.6923076) (B 0.076923065) (C 0.076923065) (D 0.076923065) (R 0.076923065)))
                    (C ((A 0.11428572) (B 0.19047621) (C 0.5714286) (D 0.095238104) (R 0.02857143))))))))


;; PPM with Blending and fixed order bound
;; ===========================================================================

(5am:def-suite ppm-blending-order-bound :in ppm)
(5am:in-suite ppm-blending-order-bound)

;; Abracadabra
(5am:test abracadabra-ppmc2
  (5am:is (and (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound 2)
                      (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound nil))
               (not (equal
                     (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound 1)
                     (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound nil))))))
          

(5am:test abracadabra-ppmc1
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound 1)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.5555555) (B 0.111111104) (C 0.111111104) (D 0.111111104) (R 0.111111104)))
                    (R ((A 0.2857143) (B 0.2857143) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (C ((A 0.18181819) (B 0.54545456) (C 0.09090909) (D 0.09090909) (R 0.09090909)))
                    (A ((A 0.2857143) (B 0.14285715) (C 0.14285715) (D 0.28571427) (R 0.14285715)))
                    (D ((A 0.21428573) (B 0.2857143) (C 0.2857143) (D 0.14285715) (R 0.071428575)))
                    (A ((A 0.42857143) (B 0.14285715) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (B ((A 0.26666665) (B 0.22222221) (C 0.22222221) (D 0.22222221) (R 0.06666666)))
                    (R ((A 0.19047621) (B 0.095238104) (C 0.047619052) (D 0.047619052) (R 0.61904764)))
                    (A ((A 0.6470587) (B 0.117647044) (C 0.058823522) (D 0.058823522) (R 0.117647044)))
                    (C ((A 0.2173913) (B 0.34782612) (C 0.17391306) (D 0.17391306) (R 0.08695652))))))))

(5am:test abracadabra-ppmc0
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures nil :update-exclusion nil :order-bound 0)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.5555555) (B 0.111111104) (C 0.111111104) (D 0.111111104) (R 0.111111104)))
                    (R ((A 0.2857143) (B 0.2857143) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (C ((A 0.33333334) (B 0.16666667) (C 0.16666666) (D 0.16666666) (R 0.16666667)))
                    (A ((A 0.2857143) (B 0.14285715) (C 0.14285715) (D 0.28571427) (R 0.14285715)))
                    (D ((A 0.37499997) (B 0.125) (C 0.125) (D 0.24999996) (R 0.125)))
                    (A ((A 0.42857143) (B 0.14285715) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (B ((A 0.49999997) (B 0.12499999) (C 0.12499999) (D 0.12499999) (R 0.12499999)))
                    (R ((A 0.44444442) (B 0.22222221) (C 0.111111104) (D 0.111111104) (R 0.111111104)))
                    (A ((A 0.40000004) (B 0.20000002) (C 0.10000001) (D 0.10000001) (R 0.20000002)))
                    (C ((A 0.45454547) (B 0.18181819) (C 0.09090909) (D 0.09090909) (R 0.18181819))))))))

;; letlettertele
(5am:test letlettertele-ppmc2
  (5am:is (equal (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures nil :update-exclusion nil :order-bound 2))))

(5am:test letlettertele-ppmc1
  (5am:is (not (equal
                (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures nil :update-exclusion nil :order-bound 1)))))

;; assanissimassa
(5am:test assanissimassa-ppmc3
  (5am:is (equal (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures nil :update-exclusion nil :order-bound 3))))

(5am:test assanissimassa-ppmc2
  (5am:is (not (equal
                (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures nil :update-exclusion nil :order-bound 2)))))

;; missisippi
(5am:test mississippi-ppmc2
  (5am:is (equal (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures nil :update-exclusion nil :order-bound 2))))

(5am:test mississippi-ppmc1
  (5am:is (not (equal
                (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures nil :update-exclusion nil :order-bound 1)))))

;; woolloomooloo
(5am:test woolloomooloo-ppmc2
  (5am:is (equal (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures nil :update-exclusion nil :order-bound nil) 
                 (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures nil :update-exclusion nil :order-bound 2))))

(5am:test woolloomooloo-ppmc1
  (5am:is (not (equal (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures nil :update-exclusion nil :order-bound nil) 
                      (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures nil :update-exclusion nil :order-bound 1)))))

;; agcgacgag
(5am:test agcgacgag-ppmc2
  (5am:is (equal (test-ppm '(a g c g a c g a g) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 (test-ppm '(a g c g a c g a g) :escape :c :mixtures nil :update-exclusion nil :order-bound 2))))

(5am:test agcgacgag-ppmc1
  (5am:is (not (equal (test-ppm '(a g c g a c g a g) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                      (test-ppm '(a g c g a c g a g) :escape :c :mixtures nil :update-exclusion nil :order-bound 1)))))
          
                 
;; PPM* with Mixtures
;; ===========================================================================

(5am:def-suite ppm*-mixtures :in ppm)
(5am:in-suite ppm*-mixtures)

;; abracadabrac (Cleary & Teahan, 1997; Bunton, 1996)
(5am:test abracadabra-ppmc*i
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.59999996) (B 0.09999999) (C 0.09999999) (D 0.09999999) (R 0.09999999)))
                    (R ((A 0.33333334) (B 0.33333334) (C 0.11111111) (D 0.11111111) (R 0.11111111)))
                    (A ((A 0.25) (B 0.25) (C 0.125) (D 0.125) (R 0.25)))
                    (C ((A 0.20000002) (B 0.53333336) (C 0.06666668) (D 0.06666668) (R 0.13333336)))
                    (A ((A 0.26666665) (B 0.19999997) (C 0.19999997) (D 0.13333331) (R 0.19999997)))
                    (D ((A 0.20833334) (B 0.2916667) (C 0.2916667) (D 0.083333336) (R 0.125)))
                    (A ((A 0.25) (B 0.1875) (C 0.1875) (D 0.1875) (R 0.1875)))
                    (B ((A 0.2093023) (B 0.21705426) (C 0.21705426) (D 0.21705426) (R 0.13953489)))
                    (R ((A 0.19148937) (B 0.14893615) (C 0.12765956) (D 0.12765956) (R 0.40425533)))
                    (A ((A 0.4347826) (B 0.1521739) (C 0.13043477) (D 0.13043477) (R 0.1521739)))
                    (C ((A 0.13157894) (B 0.1973684) (C 0.44736835) (D 0.13157894) (R 0.092105255))))))))

;; letlettertele (Larsson, 1999)
(5am:test letlettertele-ppmc*i
  (5am:is (equal (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 '((0 (L ((E 0.25) (L 0.25) (R 0.25) (T 0.25)))
                    (E ((E 0.125) (L 0.625) (R 0.125) (T 0.125)))
                    (T ((E 0.35714287) (L 0.35714287) (R 0.14285715) (T 0.14285715)))
                    (L ((E 0.2777778) (L 0.2777778) (R 0.16666667) (T 0.2777778)))
                    (E ((E 0.53125) (L 0.21875001) (R 0.09375) (T 0.15625)))
                    (T ((E 0.19444443) (L 0.19444443) (R 0.08333332) (T 0.5277778)))
                    (T ((E 0.18421054) (L 0.5526316) (R 0.07894737) (T 0.18421054)))
                    (E ((E 0.19444445) (L 0.3333333) (R 0.08333333) (T 0.3888889)))
                    (R ((E 0.14999999) (L 0.11666667) (R 0.049999997) (T 0.68333334)))
                    (T ((E 0.28) (L 0.23999998) (R 0.19999999) (T 0.28)))
                    (E ((E 0.27956992) (L 0.24731185) (R 0.16129033) (T 0.311828)))
                    (L ((E 0.12903225) (L 0.0967742) (R 0.48387095) (T 0.29032257)))
                    (E ((E 0.6153847) (L 0.13461538) (R 0.09615384) (T 0.15384616))))))))
                 
;; assanissimassa (Blelloch, 2001)
(5am:test assanissimassa-ppmc*i
  (5am:is (equal (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.19999999) (I 0.19999999) (M 0.19999999) (N 0.19999999) (S 0.19999999)))
                    (S ((A 0.59999996) (I 0.09999999) (M 0.09999999) (N 0.09999999) (S 0.09999999)))
                    (S ((A 0.33333334) (I 0.11111111) (M 0.11111111) (N 0.11111111) (S 0.33333334)))
                    (A ((A 0.1764706) (I 0.058823526) (M 0.058823526) (N 0.058823526) (S 0.64705884)))
                    (N ((A 0.23809524) (I 0.04761905) (M 0.04761905) (N 0.04761905) (S 0.61904764)))
                    (I ((A 0.3) (I 0.1) (M 0.1) (N 0.2) (S 0.3)))
                    (S ((A 0.25) (I 0.1875) (M 0.12499999) (N 0.1875) (S 0.25)))
                    (S ((A 0.30434784) (I 0.13043478) (M 0.086956516) (N 0.13043478) (S 0.34782606)))
                    (I ((A 0.4871795) (I 0.07692308) (M 0.051282052) (N 0.07692308) (S 0.30769232)))
                    (M ((A 0.14285715) (I 0.14285715) (M 0.07142857) (N 0.10714285) (S 0.53571427)))
                    (A ((A 0.19999999) (I 0.19999999) (M 0.17142856) (N 0.17142856) (S 0.25714287)))
                    (S ((A 0.17021279) (I 0.14893618) (M 0.12765957) (N 0.24468085) (S 0.30851063)))
                    (S ((A 0.17816092) (I 0.16091955) (M 0.10344828) (N 0.10344828) (S 0.454023)))
                    (A ((A 0.4527559) (I 0.19291338) (M 0.07086614) (N 0.07086614) (S 0.21259843))))))))

;; mississippi (Witten et al., 1999)
(5am:test mississippi-ppmc*i
  (5am:is (equal (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 '((0 (M ((I 0.25) (M 0.25) (P 0.25) (S 0.25))) (I ((I 0.125) (M 0.625) (P 0.125) (S 0.125)))
                    (S ((I 0.35714287) (M 0.35714287) (P 0.14285715) (S 0.14285715)))
                    (S ((I 0.2777778) (M 0.2777778) (P 0.16666667) (S 0.2777778)))
                    (I ((I 0.16666667) (M 0.16666667) (P 0.10000001) (S 0.5666667)))
                    (S ((I 0.20588236) (M 0.14705881) (P 0.08823529) (S 0.5588235)))
                    (S ((I 0.25) (M 0.11363636) (P 0.06818181) (S 0.5681818)))
                    (I ((I 0.5) (M 0.0925926) (P 0.055555556) (S 0.35185185)))
                    (P ((I 0.21428573) (M 0.11904763) (P 0.07142857) (S 0.5952381)))
                    (P ((I 0.28000003) (M 0.2) (P 0.2) (S 0.32)))
                    (I ((I 0.18421052) (M 0.13157895) (P 0.47368425) (S 0.21052632))))))))
                 
;; woolloomooloo (Williams, 1991)
(5am:test woolloomooloo-ppmc*i
  (5am:is (equal (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures nil :update-exclusion nil :order-bound nil)
                 '((0 (W ((L 0.25) (M 0.25) (O 0.25) (W 0.25)))
                    (O ((L 0.14285715) (M 0.14285715) (O 0.14285715) (W 0.5714286)))
                    (O ((L 0.19999999) (M 0.19999999) (O 0.29999998) (W 0.29999998)))
                    (L ((L 0.12499999) (M 0.12499999) (O 0.5625) (W 0.1875)))
                    (L ((L 0.18181819) (M 0.27272725) (O 0.36363637) (W 0.18181819)))
                    (O ((L 0.5714286) (M 0.14285715) (O 0.19047621) (W 0.095238104)))
                    (O ((L 0.30769232) (M 0.23076925) (O 0.30769232) (W 0.15384616)))
                    (M ((L 0.55172414) (M 0.10344827) (O 0.27586207) (W 0.06896552)))
                    (O ((L 0.25) (M 0.125) (O 0.5) (W 0.125)))
                    (O ((L 0.21739131) (M 0.21739131) (O 0.43478262) (W 0.13043478)))
                    (L ((L 0.3125) (M 0.3125) (O 0.3125) (W 0.0625)))
                    (O ((L 0.64285713) (M 0.07142857) (O 0.21428572) (W 0.07142857)))
                    (O ((L 0.20833334) (M 0.10416667) (O 0.625) (W 0.0625))))))))
                 
;; agcgacgag (Giegerich & Kurtz, 1994, 1995)
(5am:test agcgacgag-ppmc*i
  (5am:is (equal (test-ppm '(a g c g a c g a g) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 '((0 (A ((A 0.33333334) (C 0.33333334) (G 0.33333334)))
                    (G ((A 0.6666667) (C 0.16666667) (G 0.16666667)))
                    (C ((A 0.4) (C 0.2) (G 0.4)))
                    (G ((A 0.33333334) (C 0.33333334) (G 0.33333334)))
                    (A ((A 0.21052633) (C 0.5263158) (G 0.26315793)))
                    (C ((A 0.25) (C 0.19999999) (G 0.54999995)))
                    (G ((A 0.22727273) (C 0.22727273) (G 0.5454545)))
                    (A ((A 0.5483871) (C 0.25806454) (G 0.1935484)))
                    (G ((A 0.18750001) (C 0.53125006) (G 0.28125))))))))


;; PPM* with Mixtures and Update Exclusion
;; ===========================================================================

(5am:def-suite ppm-mixtures-update-exclusion :in ppm)
(5am:in-suite ppm-mixtures-update-exclusion)

;; abracadabra (Cleary & Teahan, 1997; Bunton, 1996)
(5am:test abracadabra-ppmc*ui
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion t :order-bound nil)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.59999996) (B 0.09999999) (C 0.09999999) (D 0.09999999) (R 0.09999999)))
                    (R ((A 0.33333334) (B 0.33333334) (C 0.11111111) (D 0.11111111) (R 0.11111111)))
                    (A ((A 0.25) (B 0.25) (C 0.125) (D 0.125) (R 0.25)))
                    (C ((A 0.20000002) (B 0.53333336) (C 0.06666668) (D 0.06666668) (R 0.13333336)))
                    (A ((A 0.26666665) (B 0.19999997) (C 0.19999997) (D 0.13333331) (R 0.19999997)))
                    (D ((A 0.20833334) (B 0.2916667) (C 0.2916667) (D 0.083333336) (R 0.125)))
                    (A ((A 0.25) (B 0.1875) (C 0.1875) (D 0.1875) (R 0.1875)))
                    (B ((A 0.2093023) (B 0.21705426) (C 0.21705426) (D 0.21705426) (R 0.13953489)))
                    (R ((A 0.20000002) (B 0.13333334) (C 0.13333334) (D 0.13333334) (R 0.40000004)))
                    (A ((A 0.42857143) (B 0.14285715) (C 0.14285715) (D 0.14285715) (R 0.14285715)))
                    (C ((A 0.1356784) (B 0.19095479) (C 0.44221106) (D 0.14070353) (R 0.09045227))))))))

                 
;; PPM with mixtures and fixed order bound
;; ===========================================================================

(5am:def-suite ppm-mixtures-order-bound :in ppm)
(5am:in-suite ppm-mixtures-order-bound)

(5am:test abracadabra-ppmc2i
  (5am:is (and (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound 2)
                      (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound nil))
               (not (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound 1)
                           (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound nil))))))

(5am:test abracadabra-ppmc1i
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound 1)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.59999996) (B 0.09999999) (C 0.09999999) (D 0.09999999) (R 0.09999999)))
                    (R ((A 0.33333334) (B 0.33333334) (C 0.11111111) (D 0.11111111) (R 0.11111111)))
                    (A ((A 0.25) (B 0.25) (C 0.125) (D 0.125) (R 0.25)))
                    (C ((A 0.20000002) (B 0.53333336) (C 0.06666668) (D 0.06666668) (R 0.13333336)))
                    (A ((A 0.26666665) (B 0.19999997) (C 0.19999997) (D 0.13333331) (R 0.19999997)))
                    (D ((A 0.20833334) (B 0.2916667) (C 0.2916667) (D 0.083333336) (R 0.125)))
                    (A ((A 0.25) (B 0.1875) (C 0.1875) (D 0.1875) (R 0.1875)))
                    (B ((A 0.2093023) (B 0.21705426) (C 0.21705426) (D 0.21705426) (R 0.13953489)))
                    (R ((A 0.19148937) (B 0.14893615) (C 0.12765956) (D 0.12765956) (R 0.40425533)))
                    (A ((A 0.4347826) (B 0.1521739) (C 0.13043477) (D 0.13043477) (R 0.1521739)))
                    (C ((A 0.19230768) (B 0.2884615) (C 0.19230768) (D 0.19230768) (R 0.13461538))))))))
                 
(5am:test abracadabra-ppmc0i
  (5am:is (equal (test-ppm '(a b r a c a d a b r a c) :escape :c :mixtures t :update-exclusion nil :order-bound 0)
                 '((0 (A ((A 0.19999999) (B 0.19999999) (C 0.19999999) (D 0.19999999) (R 0.19999999)))
                    (B ((A 0.59999996) (B 0.09999999) (C 0.09999999) (D 0.09999999) (R 0.09999999)))
                    (R ((A 0.33333334) (B 0.33333334) (C 0.11111111) (D 0.11111111) (R 0.11111111)))
                    (A ((A 0.25) (B 0.25) (C 0.125) (D 0.125) (R 0.25)))
                    (C ((A 0.33333334) (B 0.22222221) (C 0.111111104) (D 0.111111104) (R 0.22222221)))
                    (A ((A 0.26666665) (B 0.19999997) (C 0.19999997) (D 0.13333331) (R 0.19999997)))
                    (D ((A 0.31250003) (B 0.18750003) (C 0.18750003) (D 0.125) (R 0.18750003)))
                    (A ((A 0.25) (B 0.1875) (C 0.1875) (D 0.1875) (R 0.1875)))
                    (B ((A 0.2727273) (B 0.18181819) (C 0.18181819) (D 0.18181819) (R 0.18181819)))
                    (R ((A 0.2647059) (B 0.20588236) (C 0.1764706) (D 0.1764706) (R 0.1764706)))
                    (A ((A 0.25714287) (B 0.19999999) (C 0.17142856) (D 0.17142856) (R 0.19999999)))
                    (C ((A 0.2777778) (B 0.19444445) (C 0.16666667) (D 0.16666667) (R 0.19444445))))))))
                 
;; letlettertele
(5am:test letlettertele-ppmc2i
  (5am:is (equal (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures t :update-exclusion nil :order-bound 2))))

(5am:test letlettertele-ppmc1i
  (5am:is (not (equal (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                      (test-ppm '(l e t l e t t e r t e l e) :escape :c :mixtures t :update-exclusion nil :order-bound 1)))))

;; assanissimassa
(5am:test assanissimassa-ppmc3i
  (5am:is (equal (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures t :update-exclusion nil :order-bound 3))))

(5am:test assanissimassa-ppmc2i
  (5am:is (not (equal (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                      (test-ppm '(a s s a n i s s i m a s s a) :escape :c :mixtures t :update-exclusion nil :order-bound 2)))))

;; missisippi
(5am:test mississippi-ppmc2i
  (5am:is (equal (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures t :update-exclusion nil :order-bound 2))))

(5am:test mississippi-ppmc1i
  (5am:is (not (equal (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                      (test-ppm '(m i s s i s s i p p i) :escape :c :mixtures t :update-exclusion nil :order-bound 1)))))

;; woolloomooloo
(5am:test woolloomooloo-ppmc2i
  (5am:is (equal (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures t :update-exclusion nil :order-bound nil) 
                 (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures t :update-exclusion nil :order-bound 2))))

(5am:test woolloomooloo-ppmc1i
  (5am:is (not (equal (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures t :update-exclusion nil :order-bound nil) 
                      (test-ppm '(w o o l l o o m o o l o o) :escape :c :mixtures t :update-exclusion nil :order-bound 1)))))

;; agcgacgag
(5am:test agcgacgag-ppmc2i
  (5am:is (equal (test-ppm '(a g c g a c g a g) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                 (test-ppm '(a g c g a c g a g) :escape :c :mixtures t :update-exclusion nil :order-bound 2))))

(5am:test agcgacgag-ppmc1i
  (5am:is (not (equal (test-ppm '(a g c g a c g a g) :escape :c :mixtures t :update-exclusion nil :order-bound nil)
                      (test-ppm '(a g c g a c g a g) :escape :c :mixtures t :update-exclusion nil :order-bound 1)))))
