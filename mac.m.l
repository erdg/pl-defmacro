(m

   (de mac Lst
      ~(leq [(Name Args . Body) Lst]
         `(de ,Name Lst
            ~(leq [,Args Lst]
               ,@Body ) ) ) )

   (de mac$ Lst
      ~(leq [(Name Args . Body) Lst]
         `(de ,Name Lst
            (let [,Args Lst]
               ~(leq ,[mapbox ($syms-in Body)]
                  ,@Body ) ) ) ) )

   (de mac! Lst
      (let [(Name Args . Body) Lst]
         ~(leq [!Syms (!syms-in Args) $Syms (!syms-to-$syms !Syms)]
            `(de ,Name Lst
               (let [,Args Lst]
                  ~(leq ,[mapbox ($syms-in Body)]
                     ~(leq* ,,[letargs (list ,@$Syms) (list ,@!Syms)]
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

   (de !syms-to-$syms (Syms)
      (mapcar !sym-to-$sym Syms) )

   (de $syms-in (L)
      (uniq (filter $sym (flat L))) )

   (de !syms-in (L)
      (uniq (filter !sym (flat L))) )

   (de mapbox (Lst)
      (mapcan ~(q `(,Q1 (box))) Lst) )

   (de letargs (Xs Ys)
      (mapcan list Xs Ys) )

)

##### QUICK DOCS
#
# mac$ - defmacro/g! from Let Over Lambda
#
# mac! - defmacro! with an explicit backquote on the body
#
# See 'evolution-of-macbang.l' for a more detailed explanation
#
###


##### TESTING
#
# : (load "mac.tests.l")
#
###

