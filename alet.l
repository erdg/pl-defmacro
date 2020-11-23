(defmacro alet (Args . Body)
   "`"'(@
         (job '(","(cons 'This (cdr (last Body))) ",@"Args)
            ",@"(head -1 Body)
            (apply This (rest)) ) ) )

(let alet-test
      (alet ((Cnt . 0))
         (recur (N)
            (if (= N 'invert)
               (setq This
                  '((N)
                     (if (= N 'invert)
                        (setq This recurse)
                        (dec 'Cnt N) ) ) )
               (inc 'Cnt N) ) ) )
   (test 2 (alet-test 2))
   (test 6 (alet-test 4))
   (alet-test 'invert)
   (test -4 (alet-test 10))
   (t (prinl "alet.l -- all tests passed")) )

(def 'lambda quote)

(defmacro dlambda "Ds"
   "`"(lambda @
         (case (next)
            ",@"(make
                  (for "D" "Ds"
                     (link
                        (list (car "D")
                           (cl-backquote-form
                              "`"(apply (lambda ",@"(cdr "D")) (rest)) ) ) ) ) ) ) ) )
