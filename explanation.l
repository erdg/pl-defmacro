# NOTE - this file is for documentation only and does not work

   # the original 'defmacro' in normal picolisp

      (de defmacro Lst                    # unevaluated args
         (let [(@Nm @Args . Body) Lst]    # are destructured
            (macro
               (de @Nm Lst                # and filled in
                  (let [@Args Lst]
                     (eval
                        (quasiquote ^ Body) ) ) ) ) ) )

   # the file 'm.l' allows to use 'unquote' and 'unquote-splice' "natively" in
   # picolisp code. with that, the definition becomes

      (de defmacro Lst
         (let [(Nm Args . Body) Lst]
            (eval                         # note that '(eval (quasiquote ...))'
               (quasiquote                # is equivalent to '(macro ...)'
                  `(de ,Nm Lst
                     (let [,Args Lst]
                        (eval
                           (quasiquote ,@Body) ) ) ) ) ) ) )

   # the 'evq' read-macro was created because '(eval (quasiquote ...))' is such
   # a common pattern

      (de defmacro Lst
         (let [(Nm Args . Body) Lst]
            ~(evq
               `(de ,Nm Lst
                  (let [,Args Lst]
                     ~(evq ,@Body) ) ) ) ) )

   # the definition of 'evq' is

      (de eval-quasiquote Lst
         (macro
            '((eval (quasiquote ^ Lst))) ) )

      (def 'evq eval-quasiquote)

   # it transforms '(evq ...)' into '(eval (quasiquote ...))' before it runs.
   # as such, the previous two definitions of 'defmacro' are identical as far
   # as the picolisp interpreter is concerned.

   # note that 'evq' returns a list wrapped in an extra layer of parens. this
   # is because the tilde '~' read-macro is used to *splice* it in. the '~'
   # read-macro is used because the backquote '`' read-macro has been shadowed
   # in '.m.l' files.

   # another common pattern is '(let [...] (eval (quasiquote ...)))'. the 'leq'
   # ('let-eval-quasiquote') read-macro was created as an abbreviation. 'leq'
   # allows to concisely express quasiquote macros with variable bindings using
   # the same syntax as 'let'.

   # the implementation of 'leq' is

      (de let-eval-quasiquote Lst
         (let [(Args . Body) (leqargs Lst)]
            (macro
               '((let ^ Args (eval (quasiquote ^ Body )))) ) ) )

      (de leqargs (Lst)
         (let [L (_leqargs Lst)
               I (index (find pair L) L)
               Args (head I L)
               Body (tail (- I) L)]
            (macro '((^ Args) ^ Body)) ) )

      (de _leqargs (Lst)
         (if (atom (car Lst))
            (cons (car Lst) (_leqargs (cdr Lst)))
            Lst ) )

   # it works the same as the 'evq' read-macro, but requires some extra
   # processing of arguments to correctly parse the (possibly unquoted) 'let'
   # args and body

   # using 'leq', the definition of 'defmacro' becomes

      (de mac Lst
         ~(leq [(Nm Args . Body) Lst]
            `(de ,Nm Lst
               ~(leq [,Args Lst]
                  ,@Body) ) ) )

   # now that is a powerful and concise piece of code! and once again, because
   # 'leq' is a read-macro that transforms the raw list structure, this
   # definition is the same as the previous few.

   # note that 'defmacro' was renamed 'mac' to better fit in with picolisp
   # naming conventions.

   # 'mac' can be used in '.m.l' files to define macros

      (mac aif (Test Then Else)
         `(let it ,Test
               (if it ,Then ,Else) ) )

   # NOTE - 'aif' is merely an example macro and  would never be used in
   # picolisp because the same functionality exists in the built-in 'if'.

   # 'aif' is an "anaphoric" macro that captures the symbol 'it' and binds it
   # to the result of the 'Test' expression so it can be refered to without
   # having to explicitly create new variables

      (de add2-safe (N)
         (aif (num? N)
            (+ 2 it)
            (prinl "ERROR - " it " is NOT a number")) )

   # as expected, the definition of 'aif' has the '(eval (quasiquote ...))'
   # pattern

      (Lst
         (let ((Test Then Else) Lst)
            (eval
               (quasiquote
                  "`"(let it "," Test
                        (if it "," Then "," Else) ) ) ) ) )

   # if 'aif' is going to be used heavily (say to "add2cents-safely" to like 50
   # billion bank transactions a day), a serious run-time performance hit will
   # be taken. 'eval' and 'quasiquote' are pretty expensive operations.

   # but there's nothing in 'aif's run-time code that will require
   # 'quasiquote'. remember that "eval quasiquote" forms are just a different
   # way of writing picolisp 'macro's. it turns out that a "macro compiler" can
   # be written which turns "eval quasiquote" macros into normal picolisp
   # 'macro's by "precomputing" the translation.

   # the exact function of 'quasiquote' is to do this translation. the
   # translation happens in two parts. first 'quasiquote' passes the list to
   # the internal function '_quasiquote'.

      (_quasiquote
         "`"(let it "," Test
               (if it "," Then "," Else) ) )

   # '_quasiquote' does the actual transformation on the raw list structure. at
   # this point nothing has been evaluated and '_quasiquote' returns the
   # translation

      '(let it ^ (list Test)
         (if it ^ (list Then) ^ (list Else)) )

   # 'quasiquote' then passes this list to 'macro' to fill in the values. the
   # translation ('_quasiquote') is distinct from the execution ('quasiquote'),
   # and thus can be "precomputed" and the macro can be redefined.

   # this is exactly what 'mc' (the macro compiler) does. the idea is to pipe
   # the macro definition to low-level 'read'

      (eval
         (quasiquote
            "`"(let it "," Test
                  (if it "," Then "," Else) ) ) )

   # transform "eval quasiquote" forms into "macro _quasiquote" forms with a
   # backquote readmacro

      (macro
         `(_quasiquote
            "`"(let it "," Test
                  (if it "," Then "," Else) ) ) )

   # bring it back to lisp so the backquote readmacro fires

      (macro
         '(let it ^ (list Test)
            (if it ^ (list Then) ^ (list Else)) )

   # pipe *that* back to low-level 'read' to remove the quotes from the 'macro'
   # lists and finally bring it back to lisp where it can run like normal

      (macro
         (let it ^ (list Test)
            (if it ^ (list Then) ^ (list Else)) )

   # compiled macros tend to be an order of magnitude faster. the compiler
   # could be improved by further "compiling" to more efficient forms that
   # don't use 'macro', but that is beyond the scope of this article.

   # 'mc' can be used to define a compiled version of a macro

      (def 'aifc (mc aif))

   # or simply redefine the macro

      (def 'aif (mc aif))


   # so what's the point of all this? ostensibly it's to allow to use a more
   # convenient syntax for code transformations

      (mac aif (Test Then Else)
         `(let it ,Test
               (if it ,Then ,Else) ) )

   # vs

      (de aif Lst
         (let [(Test Then Else) Lst]
            (macro
               (let it ^ (list Test)
                  (if it ^ (list Then) ^ (list Else)) ) ) ) )

   # while it's not much of an improvement for such a simple macro, it does
   # make it easy to write more complex "Common Lisp Style" macros

      (mac defunits (Quantity Base . Units)
         `(mac ,(symb 'unit-of- Quantity) ("Val" "Unit")
            `(* ,,"Val"
               (case ,,"Unit"
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

   # it could be argued whether or not such "macros" have any place in
   # picolisp or if they should be written with more conventional (and
   # efficient) picolisp idioms...

   # so more realistically, the point of all this is to show just how flexible
   # and powerful the picolisp system is and how easy it is to extend the
   # language. A pretty impressive (toy) Common Lisp Macro System can be added to
   # picolisp with roughly 300 lines of code
