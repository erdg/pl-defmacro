(let A (alet (Cnt 0)
         (recur (N)
            (if (= N 'invert)
               (setq This
                  '((N)
                     (if (= N 'invert)
                        (setq This recurse)
                        (dec 'Cnt N) ) ) )
               (inc 'Cnt N) ) ) )

   (test 2 (A 2))
   (test 6 (A 4))
   (A 'invert)
   (test -4 (A 10)) )

(let A (alet (Cnt 0)
         '`(dlambda
               ("inc" (N) (inc 'Cnt N))
               ("dec" (N) (dec 'Cnt N)) ) )

   (test 2 (A "inc" 2))
   (test 6 (A "inc" 4))
   (test -4 (A "dec" 10)) )

(t (prinl "alet.l -- all tests passed"))
