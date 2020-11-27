(m

   (mac defunits1 (Quantity Base . Units)
      `(mac! ,(symb 'unit-of- Quantity) (!Val !Unit)
         `(* ,,$Val
            (case ,,$Unit
               (,Base 1)
               ,@(mapcar
                     ~(qfn `(,(car Q1) ,(cadr Q1)))
                     (groups 2 Units) ) ) ) ) )

   (mac defunits (Quantity Base . Units)
      `(mac! ,(symb 'unit-of- Quantity) (!Val !Unit)
         `(* ,,$Val
            (case ,,$Unit
               (,Base 1)
               ,@(mapcar
                     ~(qfn (X)
                        `(,(car X)
                           ,(defunits-chaining
                              (car X)
                                 (cons
                                    (quasiquote `(,Base 1))
                                    (groups 2 Units) ) ) ) )
                     (groups 2 Units) ) ) ) ) )

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
