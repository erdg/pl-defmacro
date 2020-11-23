(m

   (de defmacro-with-boxes Lst
      (let [(Name Args . Body) Lst
            Syms (uniq (filter !sym (flat Body))) ]
         (eval
            (quasiquote
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,(mapcan
                              '((S) (quasiquote `(,S (box))))
                              Syms )
                        (eval
                           (quasiquote
                              ,@Body ) ) ) ) ) ) ) ) )

   (de !sym (S) (= '! (car (chop S))))

   (de flat (L) (fish atom L))

)
