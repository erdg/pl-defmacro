(m

   (de defmacro-bang Lst
      (let ((Name Args . Body) Lst
            Os (filter o!sym Args)
            !s (mapcar o!sym->!sym Os))
         (eval
            (quasi
               `(defmacro-with-boxes ,Name ,Args
                  '(let ,,(mapcan list (list ,@!s) (list ,@Os))
                     ,@Body) ) ) ) ) )

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

   (de o!sym (S)
      (and (sym? S)
         (> (length S 2))
         (pre? 'O! S) ) )

   (de o!sym->!sym (S)
      (any (pack (cdr (chop S)))) )

   (de !sym (S) (= '! (car (chop S))))

   (de flat (L) (fish atom L))

   (de eval-quasiquote Lst
      (macro
         (eval
            (quasiquote ^ Lst) ) ) )

   (de mac! Lst
      (let ((Name Args . Body) Lst
            !s (filter !sym Args)
            $s (mapcar !sym-to-$sym !s))
         (eval
            (quasi
               `(defmacro-with-boxes ,Name ,Args
                  '(let ,,(mapcan list (list ,@$s) (list ,@!s))
                     ,@Body) ) ) ) ) )

   (de mac$ Lst
      (let [(Name Args . Body) Lst
            Syms (uniq (filter $sym (flat Body))) ]
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

   (de !sym (S)
      (and (sym? S)
         (> (length S 1))
         (pre? '! S) ) )

(de $sym (S)
      (and (sym? S)
         (> (length S 1))
         (pre? '$ S) ) )

   (de !sym-to-$sym (S)
      (any (pack '$ (cdr (chop S)))) )

)
