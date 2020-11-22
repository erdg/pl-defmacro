(mload "mac.tests.m.l")

# duplicates to check for super weird "self-modifying code" bug

(test 'Z (nif$ 0 'P 'Z 'N))
(test 'Z (nif$ 0 'P 'Z 'N))

(test 'Z (nif! 0 'P 'Z 'N))
(test 'Z (nif! 0 'P 'Z 'N))

(test 'Z (nif!2 0 'P 'Z 'N))
(test 'Z (nif!2 0 'P 'Z 'N))

(test 9  (let Cnt 2 (square (inc 'Cnt))))
(test 9  (let Cnt 2 (square (inc 'Cnt))))
(test 12 (let Cnt 2 (square-buggy (inc 'Cnt))))
(test 12 (let Cnt 2 (square-buggy (inc 'Cnt))))

(test 7 (let X 2 (add (inc 'X) (inc 'X))))
(test 7 (let X 2 (add (inc 'X) (inc 'X))))

(t (prinl "mac[!|$] -- all tests passed"))
