(m

   (de mac Lst
      (let [(Nm Args . Body) Lst]
         (evq
            `(macro
               (de ,Nm Lst
                  (let [,Args Lst]
                     (evq ,@Body) ) ) ) ) ) )

   (de mac$ Lst
      (let [(Name Args . Body) Lst]
         (evq
            `(de ,Name Lst
               (let [,Args Lst]
                  (let ,[mapcan ~(q `(,Q1 (box))) ($syms-in Body)]
                     (evq ,@Body ) ) ) ) ) ) )

   (de mac! Lst
      (let [(Name Args . Body) Lst]
         (let [!Args (!syms-in Args) $Args (!syms-to-$syms !Args)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,[mapbox ($syms-in Body)]
                        ~(enq
                           `(let ,,[letargs (list ,@$Args) (list ,@!Args)]
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )

   (de maq! Lst
      (let [(Name Args . Body) Lst]
         (let [!Syms (!syms-in Args)  $Syms (!syms-to-$syms !Syms)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,[mapbox ($syms-in Body)]
                        ~(enq
                           `(let ,,[letargs (list ,@$Syms) (list ,@!Syms)]
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

### QUICK DOCS
#
# mac$ - defmacro/g! from Let Over Lambda
#
# maq! - defmacro! with an implicit backquote on the body
#
#     e.g. (maq! square (!X) (* ,$X ,$X))
#
# mac! - defmacro! with an explicit backquote on the body
#
#     e.g. (mac! square (!X) `(* ,$X ,$X))
#
#
# See 'evolution-of-macbang.l' for a more detailed explanation
#
###


### TESTING
#
# : (load "mac.tests.l")
#
###

