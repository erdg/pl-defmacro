(m

   (mac! nif! (!Expr Pos Zero Neg)
      `(cond
         ((gt0 ,$Expr) ,Pos)
         ((=0 ,$Expr) ,Zero)
         (T ,Neg) ) )

   (mac!2 nif!2 (!Expr Pos Zero Neg)
      (cond
         ((gt0 ,$Expr) ,Pos)
         ((=0 ,$Expr) ,Zero)
         (T ,Neg) ) )

   (mac!3 nif!3 (!Expr Pos Zero Neg)
      (cond
         ((gt0 ,$Expr) ,Pos)
         ((=0 ,$Expr) ,Zero)
         (T ,Neg) ) )

   (mac$ nif$ (Expr Pos Zero Neg)
      `(let ,$Res ,Expr
         (cond
            ((gt0 ,$Res) ,Pos)
            ((=0  ,$Res) ,Zero)
            (T ,Neg) ) ) )

   (mac! square (!X)
      `(* ,$X ,$X) )

   (mac square-buggy (X)
      `(* ,X ,X) )

   (mac! add (!X !Y) `(+ ,$X ,$Y))

   (mac add-buggy (X Y) `(+ ,X ,Y))

   (mac! add3 (!X !Y Z)
      `(let ,$Z ,Z
         (+ ,$X ,$Y ,$Z) ) )

   (mac! add3-buggy (X Y Z)
      `(+ ,X ,Y ,Z) )

)
