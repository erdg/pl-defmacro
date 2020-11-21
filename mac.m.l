(m
   (de mac! Lst
      (let [(Name Args . Body) Lst]
         (let [!s (filter !sym Args)  $s (mapcar !sym-to-$sym !s)]
            (evq
               `(mac$ ,Name ,Args
                  '(let ,,(mapcan list (list ,@$s) (list ,@!s))
                     ,@Body) ) ) ) ) )

   (de mac$ Lst
      (let [(Name Args . Body) Lst]
         (let [Syms (uniq (filter $sym (flat Body)))]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst  ,(mapcan '((S) (quasi `(,S (box)))) Syms)]
                     (evq
                        ,@Body ) ) ) ) ) ) )

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

   (de flat (L) (fish atom L))

)

# : (load "mac.tests.l")

