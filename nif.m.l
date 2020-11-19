(m
   (defmacro-with-boxes nif (Expr Pos Zero Neg)
      `(let ,Res! ,Expr
         (cond ((gt0 ,Res!) ,Pos)
               ((=0  ,Res!) ,Zero)
               (T ,Neg) ) ) )
   
   (test "zero" (nif 0 "pos" "zero" "neg"))
)
