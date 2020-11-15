# Just like Common Lisp! lol
(defmacro aif (Test Then Else)
   "`"(let it ","Test
         (if it ","Then ","Else) ) )

(defmacro awhen (Test . Body)
   "`"(aif ","Test
         (prog ",@"Body) ) )

(defmacro aand Args
   (cond ((not Args) T)
         ((not (cdr Args)) (car Args))
         (T "`"(aif ","(car Args) (aand ",@"(cdr Args)))) ) )

# Common Lisp (from On Lisp p.191)

# (defmacro aif (test-form then-form &optional else-form)
#    `(let ((it ,test-form))
#       (if it ,then-form ,else-form)))

# (defmacro awhen (test-form &body body)
#    `(aif ,test-form
#       (progn ,@body)))

# (defmacro aand (&rest args)
#    (cond ((null args) t)
#          ((null (cdr args)) (car args))
#          (t `(aif ,(car args) (aand ,@(cdr args))))))

# from On Lisp (p. 219)
(defmacro a+ Args (a+expand Args NIL))

(de a+expand (Args Syms)
   (if Args
      (let Sym (box)
         (cl-backquote-form
            "`"(let [","Sym ","(car Args)  it ","Sym]
                  ","(a+expand (cdr Args)
                        (append Syms (list Sym)) ) ) ) )
      (cl-backquote-form
         "`"(+ ",@"Syms) ) ) )

(scl 2)

# super real world uses of 'a+'
(de total-cost-of-eating-at-a-restaurant-in-massachusetts (Price)
   (format
      (a+ Price (*/ it 0.05 1.0) (*/ it 3.0 1.0))
      *Scl ) )

(de total-cost-of-eating-at-a-restaurant-in-massachusetts-in-1993-adjusted-for-2020 (MenuPrice)
   (format
      (a+ (any (total-cost-of-eating-at-a-restaurant-in-massachusetts MenuPrice)) (*/ it 0.8 1.0))
      *Scl ) )

(defmacro alist Args (alist-expand Args NIL))

(de alist-expand (Args Syms)
   (if Args
      (let Sym (box)
         (cl-backquote-form
            "`"(let [","Sym ","(car Args)  it ","Sym]
                  ","(alist-expand (cdr Args)
                        (append Syms (list Sym)) ) ) ) )
      (cl-backquote-form
         "`"(list ",@"Syms) ) ) )

# from On Lisp (p.220)
(defmacro defanaph (Name)
   "`"(defmacro ","Name Args
         (anaphex Args (list (cons 'quote ","(pop-symbol Name)))) ) )

(de anaphex (Args Expr)
   (if Args
      (let Sym (box)
         (cl-backquote-form
            "`"(let [","Sym ","(car Args)  it ","Sym]
                  ","(anaphex (cdr Args)
                        (append Expr (list Sym)) ) ) ) )
      Expr ) )

(de pop-symbol (Sym)
   (any (pack (cdr (chop Sym)))) )

(defanaph a*)
(defanaph amapcar)

# TESTS
(test '(let Y 7 (* Y 2 3 4 5))
   (let [N 7
         L (2 3 4 5) ]
      (cl-backquote-form
         "`"(let Y ","N
               (* Y ",@"L) ) ) ) )

(test 10 (aand (+ 1 2) (+ 3 it) (+ 4 it)))

(test (1 2 3)
   (aand 1 (list it) (cons 2 it) (cons 3 it) (reverse it)) )

(test 48 (a* 2 (+ 2 it) (+ 2 it)))

(test "17.19"
   (total-cost-of-eating-at-a-restaurant-in-massachusetts-in-1993-adjusted-for-2020 7.95) )

(test (2 4 6) (alist 2 (+ 2 it) (+ 2 it)))

# more lols, this is probably the mose useful programming pattern yet discovered
(test (7 17 27)
   (amapcar +
      (list (it 1 2 3) (it 4 5 6) (it 7 8 9))
      (1 2 3) ) )

(test (6 240 1512)
   (amapcar *
      (list (it 1 2 3) (it 4 5 6) (it 7 8 9))
      (1 2 3) ) )

(test (36 14400 254016)
   (amapcar *
      (list (it 1 2 3) (it 4 5 6) (it 7 8 9))
      (append it (21 23 25)) ) )

(t (prinl "defmacro.l -- all tests passed"))
