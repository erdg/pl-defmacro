(m

   (mac! nif! (!Expr Pos Zero Neg)
      `(cond
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

)
