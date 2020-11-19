(m

   (mac defunits (Quantity Base . Units)
      `(mac ,(symb 'unit-of- Quantity) ("Val" "Unit")
         (* ,,"Val"
            (case ,,"Unit"
               (,Base 1)
               ,@(mapcar
                     '((X) (quasi `(,(car X) ,(cadr X))))
                     (groups-of 2 Units) ) ) ) ) )

   (defmacro defunits2 (Quantity Base . Units)
      `(defmacro ,(symb 'unit-of- Quantity) ("Val" "Unit")
         (* ,,"Val"
            (case ,,"Unit"
               (,Base 1)
               ,@(mapcar
                     (lambda (X)
                        (quasiquote
                           `(,(car X)
                              ,(defunits-chaining
                                 (car X)
                                 (cons
                                    (quasiquote `(,Base 1))
                                    (groups-of 2 Units) ) ) ) ) )
                     (groups-of 2 Units) ) ) ) ) )

   (mac defunits3 (Quantity Base . Units)
      `(mac ,(symb 'unit-of- Quantity) ("Val" "Unit")
         (* ,,"Val"
            (case ,,"Unit"
               (,Base 1)
               ,@(mapcar
                     '((X)
                        (quasiquote
                           `(,(car X)
                              ,(defunits-chaining
                                 (car X)
                                 (cons
                                    (quasiquote `(,Base 1))
                                    (groups-of 2 Units) ) ) ) ) )
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

   (def 'lambda quote)

   (de symb @
      (any (apply pack (rest))) )

   (de groups-of (N Src)
      (if (=0 N) (err "zero length"))
      (inc 'N)
      (recur (Src Acc)
         (let Rest (nth Src N)
            (if (pair Rest)
               (recurse Rest (cons (head (- N 1) Src) Acc))
               (flip (cons Src Acc)) ) ) ) )

)
