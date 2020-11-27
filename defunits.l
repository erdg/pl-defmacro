(load "defmacro.l" "utils.l")

(def 'lambda quote)

(defmacro defunits (Quantity Base . Units)
   "`"(defmacro ","(symb 'unit-of- Quantity) (Val Unit)
      "`"(* ","","Val
            (case ","","Unit
               (","Base 1)
               ",@"(mapcar (lambda (X)
                              (quasiquote
                                 "`"(","(car X) ","(cadr X)) ) )
                           (groups 2 Units) ) ) ) ) )

# TESTS
(defunits distance in
   ft 12
   yd 36 )

(defunits time s
   m 60
   h 3600
   d 86400 )

(test 24 (unit-of-distance 2 'ft))
(test 48 (unit-of-distance 4 'ft))
(test 144 (unit-of-distance 4 'yd))

(test 1209600 (unit-of-time 14 'd))
(test 1209600 (unit-of-time 14 'd))

(t (prinl "defunits.l -- all tests passed"))
