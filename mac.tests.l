(mload "mac.tests.m.l")

# duplicates to check for super weird "self-modifying code" bug

(test 'Z (nif$ 0 'P 'Z 'N))
(test 'Z (nif$ 0 'P 'Z 'N))

(test 'Z (nif! 0 'P 'Z 'N))
(test 'Z (nif! 0 'P 'Z 'N))

(test 9  (let Cnt 2 (square (inc 'Cnt))))
(test 9  (let Cnt 2 (square (inc 'Cnt))))
(test 12 (let Cnt 2 (square-buggy (inc 'Cnt))))
(test 12 (let Cnt 2 (square-buggy (inc 'Cnt))))

(test 7 (let X 2 (add (inc 'X) (inc 'X))))
(test 7 (let X 2 (add (inc 'X) (inc 'X))))

(test 10 (aand (+ 1 2) (+ 3 it) (+ 4 it)))

(test (1 2 3)
   (aand 1 (list it) (cons 2 it) (cons 3 it) (reverse it)) )

(test 4 (aif 2 (+ 2 it)))
(test 4 (awhen 2 (+ 2 it)))

(defunits time s m 60 h (60 m) d (24 h) y (365 d))
(test 3600 (unit-of-time 1 'h))
(test 3600 (unit-of-time 1 'h))

(def 'nif (mc nif!))
(test 'z (nif 0  'p 'z 'n))

(defunits distance in
   ft 12
   yd (3 ft)
   fathom (2 yd)
   mile (5280 ft)
   rod (15 ft)
   furlong (40 rod)
   chain (4 rod)
   league (3 mile)
   cubit 18 )

(def 'unit-of-distance-compiled (mc unit-of-distance))

(test 1080 (unit-of-distance-compiled 6 'rod))

(t (prinl "mac.m.l -- all tests passed"))
