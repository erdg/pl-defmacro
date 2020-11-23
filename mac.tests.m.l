(m

   (mac$ nif$ (Expr Pos Zero Neg)
      `(let ,$Res ,Expr
         (cond
            ((gt0 ,$Res) ,Pos)
            ((=0  ,$Res) ,Zero)
            (T ,Neg) ) ) )

   (mac! nif! (!Expr Pos Zero Neg)
      `(cond
         ((gt0 ,$Expr) ,Pos)
         ((=0 ,$Expr) ,Zero)
         (T ,Neg) ) )

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

   (mac aif (Test Then Else)
      `(let it ,Test
            (if it ,Then ,Else) ) )

   (mac awhen (Test . Body)
      `(aif ,Test
            (prog ,@Body) ) )

   (mac aand Args
      (cond ((not Args) T)
            ((not (cdr Args)) (car Args))
            (T `(aif ,(car Args) (aand ,@(cdr Args)))) ) )

   (mac defunits (Quantity Base . Units)
      `(mac! ,(symb 'unit-of- Quantity) (!Val !Unit)
         (* ,,$Val
            (case ,,$Unit
               (,Base 1)
               ,@(mapcar
                     ~(qfn (X)
                        `(,(car X)
                           ,(defunits-chaining
                              (car X)
                                 (cons
                                    (quasi `(,Base 1))
                                    (groups-of 2 Units) ) ) ) )
                     (groups-of 2 Units) ) ) ) ) )

   (de defunits-chaining (U Units)
      (let Spec (assoc U Units)
         (ifn Spec
            (prinl (text "Unknown unit @1" U))
            (let Chain (cadr Spec)
               (if (pair Chain)
                  (* (car Chain)
                     (defunits-chaining
                        (cadr Chain)
                        Units ) )
                  Chain ) ) ) ) )

)
