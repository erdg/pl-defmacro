(load "defmacro.l")

# utils
(def 'lambda quote)

(de symb @
   (any (apply pack (rest))) )

(de groups-of (N Src)
   (if (=0 N) (err "zero length"))
   # hack to account for off by 1
   (inc 'N)
   (recur (Src Acc)
      (let Rest (nth Src N)
         (if (pair Rest)
            (recurse Rest (cons (head (- N 1) Src) Acc))
            (flip (cons Src Acc)) ) ) ) )


# from Let Over Lambda (p. 114)
(defmacro defunits (Quantity Base . Units)
   "`"(defmacro ","(symb 'unit-of- Quantity) (Val Unit)
        "`"(* ","","Val
               ","(cl-backquote-form
                     "`"(case ","","Unit
                           (","Base 1)
                           ",@"(mapcar (lambda (X)
                                          (cl-backquote-form
                                             "`"(","(car X) ","(cadr X)) ) )
                                       (groups-of 2 Units) ) ) ) ) ) )


# TESTS
(defunits distance in
   ft 12
   yd 36 )

(defunits time s
   m 60
   h 3600
   d 86400 )

(test 24 (unit-of-distance 2 'ft))

(test 1209600 (unit-of-time 14 'd))

(t (prinl "defunits.l -- all tests passed"))
