(m

   (de defmacro-with-boxes Lst
      (let [(Name Args . Body) Lst
            Syms (uniq (filter sym!p (flat Body))) ]
         (eval
            (quasiquote
               `(de ,Name ,Args
                  (let ,(mapcan
                           '((S) (quasiquote `(,S (box))))
                           Syms )
                     (eval
                        (quasiquote
                           ,@Body ) ) ) ) ) ) ) )

   (de sym!p (S) (= "\"!\"" (last (chop S))))

   (de flat (L) (fish atom L))

)
