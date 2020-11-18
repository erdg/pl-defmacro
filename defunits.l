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
#
# NOTE - this works, but has a weird heisenbug. After calling defunits, eg.
#
#   (defunits time s
#      m 60
#      h 3600
#      ... )
#
#   (pretty unit-of-time)  # everything looks good
#
#   (unit-of-time 3 m)
#   -> 180
#
#   (pretty unit-of-time)  # extra case statement spliced in definition!
#
#   (unit-of-time 4 h)  # process hangs
#
# the resulting function ('unit-of-time') can be used exactly once. After the
# first use, it looks like the 'case' statement (see definition below) is
# spliced into the function body *again*.
#
# This is the one place where the PL and CL versions are different...
(defmacro defunits (Quantity Base . Units)
   "`"(defmacro ","(symb 'unit-of- Quantity) (Val Unit)
        "`"(* ","","Val
               ","(cl-backquote-form         # problem
                     "`"(case ","","Unit     # area
                           (","Base 1)
                           ",@"(mapcar (lambda (X)
                                          (cl-backquote-form
                                             "`"(","(car X) ","(cadr X)) ) )
                                       (groups-of 2 Units) ) ) ) ) ) )

# fixed! I was trying way to hard with nested backquotes
# I like this version better (see defunits.m.l)
# "implicit nested backquotes" in the picolisp version make
# way more sense to me than common lisp nested backquotes
(defmacro defunits% (Quantity Base . Units)
   "`"(defmacro ","(symb 'unit-of- Quantity) (Val Unit)
        (* ","","Val
               (case ","","Unit
                     (","Base 1)
                     ",@"(mapcar (lambda (X)
                                    (cl-backquote-form
                                       "`"(","(car X) ","(cadr X)) ) )
                                 (groups-of 2 Units) ) ) ) ) )


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
