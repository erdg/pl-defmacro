(mload "mac.tests.m.l")

(test 'Z (nif! 0 'P 'Z 'N))
(test 'Z (nif$ 0 'P 'Z 'N))

(test 9  (let Cnt 2 (square (inc 'Cnt))))
(test 12 (let Cnt 2 (square-buggy (inc 'Cnt))))

(t (prinl "mac[!|$] -- all tests passed"))
