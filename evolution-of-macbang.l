# NOTE - this file is for documentation only and will not work
(m

   ### mac$ - defmacro/g! from Let Over Lambda
   #
   # this is the original definition of 'mac$' and works as expected

   (de mac$1 Lst
      (let [(Name Args . Body) Lst]
         (let [$Syms (uniq (filter $sym (flat Body)))]
            (eval
               (quasiquote
                  `(de ,Name Lst
                     (let [,Args Lst]
                        (let ,[mapcan '((S) (quasi `(,S (box)))) $Syms)]
                           (eval
                              (quasiquote
                                 ,@Body ) ) ) ) ) ) ) ) )

   # the symbol '$' was chosen because the function 'box' returns anonymous
   # transient symbols (picolisp gensyms) as e.g.  '$1723986599283'

   # (eval (quasiquote ...) happens a lot in this system, so 'evq' was created
   # to reduce noise in the definitions

   (de mac$2 Lst
      (let [(Name Args . Body) Lst]
         (let [$Syms (uniq (filter $sym (flat Body)))]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let [,(mapcan '((S) (quasi `(,S (box)))) $Syms)]
                        (evq
                           ,@Body ) ) ) ) ) ) ) )

   # pleased with the above, i wrote 'mac!', the PL translation of 'defmacro!'
   # from LOL

   ### - mac!
   #
   # this kinda worked but was totally broken on the inside. it didn't quite
   # generate the right expansion. it passed simple tests, but didn't work when
   # there was more than one "!" symbol in the form.

   (de mac!1 Lst
      (let [(Name Args . Body) Lst]
         (let [!s (filter !sym Args)  $s (mapcar !sym-to-$sym !s)]
            (evq
               `(mac$ ,Name ,Args
                  '(let ,,(mapcan list (list ,@$s) (list ,@!s))
                     ,@Body) ) ) ) ) )

   # i decided to scrap the idea of the 'mac!' calling 'mac$' (as in LOL)
   # and included the definition of 'mac$' in the definition of 'mac!'.

   # this works as expected

   (de mac!2 Lst
      (let [(Name Args . Body) Lst]
         (let [!Syms (filter !sym Args)  $Syms (mapcar !sym-to-$sym !Syms)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,(mapcan '((S) (quasi `(,S (box)))) (uniq (filter $sym (flat Body))))
                        (evq
                           '(let ,,(mapcan list (list ,@$Syms) (list ,@!Syms))
                              (evq
                                 ,@Body ) ) ) ) ) ) ) ) ) )

   # but i don't like how the code looks. it's dense and hard to parse. the
   # following definitions sculpt this into something more concise and easier
   # to understand.

   # i was frustrated that nested backquotes weren't working in this system,
   # which resulted in the creation of a read-macro 'evq*' to make writing
   # "nested" backquotes look cooler (and like they kinda worked).

   (de mac!3 Lst
      (let [(Name Args . Body) Lst]
         (let [!Syms (filter !sym Args)  $Syms (mapcar !sym-to-$sym !Syms)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,(mapcan '((S) (quasi `(,S (box)))) (uniq (filter $sym (flat Body))))
                        ~(evq*   # NEW
                           `(let ,,(mapcan list (list ,@$Syms) (list ,@!Syms))
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )

   # it's a hack though- nested backquotes don't really work, because backquote
   # doesn't really do anything in this system besides add a literal 'quote to
   # a list. oh well, we can still write crazy macros.

   # so now i'm thinking about read-macros. how do i execute code at read-time
   # after shadowing the backquote read-macro for these PL extensions?
   # fortunately, the tilde '~' read-macro is still available. who needs two
   # read-macros anyway?  if you've got one that can splice, you've got one
   # that can place (by writing functions that 'list' their results).

   # the 'q' read-macro is just that. its the sharp-backquote of this PL macro
   # lang. 'q' accepts 'Qsym' anaphors (Q1 ... QN) and returns a function with
   # things spliced and diced in.

   # ~(q `(,Q1 (box))) -> '((Q1) (quasiquote "`"(","Q1 (box))))

   (de mac!4 Lst
      (let [(Name Args . Body) Lst]
         (let [!Args (filter !sym Args) $Args (mapcar !sym-to-$sym !Args)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst] # NEW
                     (let ,[mapcan ~(q `(,Q1 (box))) (uniq (filter $sym (flat Body)]
                        ~(evq*
                           `(let ,,[mapcan list (list ,@$Args) (list ,@!Args)]
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )

   # then i wrote a bunch of small utility functions to hide the mapping and
   # filtering and provide more descriptive names for the underlying processes.
   # (see bottom of file)

   (de mac!5 Lst
      (let [(Name Args . Body) Lst]
                     # NEW                   # NEW
         (let [!Args (!syms-in Args)  $Args (!syms-to-$syms !Args)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                           # NEW
                     (let ,[mapbox ($syms-in Body)]
                        ~(evq*     # NEW
                           `(let ,,[letargs (list ,@$Args) (list ,@!Args)]
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )

   # i still wasn't happy with the explicit 'evq' forms, so the 'leq'
   # read-macro was created. 'leq' stands for "let eval quasiquote". it simply
   # rolls all three functions into a 'let' variation to make writing
   # macro-writing macros even more concise.

   (de mac!6 Lst
      (let [(Name Args . Body) Lst]
         # NEW
         ~(leq [!Syms (!syms-in Args)  $Syms (!syms-to-$syms !Syms)]
            `(de ,Name Lst
               (let [,Args Lst]
                  (let ,[mapbox ($syms-in Body)]
                     ~(evq*
                        `(let ,,[letargs (list ,@$Syms) (list ,@!Syms)]
                            (evq ,@Body ) ) ) ) ) ) ) ) )

   # finally, the 'leq*' read-macro is to 'leq' as 'evq*' is to 'evq'. This
   # brings us to the current definition and explanation.


   ###### mac! - a macro-writing macro
   #
   #  (mac! square (!X)    # args prefixed with '!' are evaluated once only
   #     `(* ,$X ,$X) )    # and referred to as prefixed with '$'
   #
   # this prevents multiple evaluation of arguments (which can produce
   # unexpected results when side-effects are involved.
   #
   #  : (let Cnt 2 (square (inc 'Cnt))
   #  -> 9
   #
   # compare to a definition with 'mac'
   #
   #  (mac square-buggy (X) `(* ,X ,X))
   #
   #  : (let Cnt 2 (square-buggy (inc 'Cnt)))   # (inc 'Cnt) evaluated twice
   #  ->12
   #

   (de mac! Lst
      (let [(Name Args . Body) Lst]
         ~(leq [!Syms (!syms-in Args)  $Syms (!syms-to-$syms !Syms)]
            `(de ,Name Lst
               (let [,Args Lst]
                  ~(leq ,[mapbox ($syms-in Body)]
                     # NEW
                     ~(leq* ,,[letargs (list ,@$Syms) (list ,@!Syms)]
                         ,@Body ) ) ) ) ) ) )

   # 'mac!' does this by gathering the !args and creating the corresponding
   # $syms.  it then searches the body of the macro being defined for any
   # additional $syms.  all the $syms are then bound to 'box'es in the
   # expansion. finally - when the defined macro is called (hence the nested
   # backquotes) - the result of evaluting the !args provided to the are bound
   # to the $sym 'box'es.
   #
   # in other words, 'mac!' creates a bunch of variable bindings in the macro
   # it defines (some of which are are finally filled in during *that* macro's
   # runtime) to provide us with a convenient way of handling variable capture
   # and execution when writing macros.
   ###


   # at this point it's all a matter of aesthetics. that and showing how it's
   # possible to completely change and extend the behavior of the PicoLisp
   # programming language. yay metaprogramming!

   ### utils

   (de !sym (S)
      (and (sym? S)
         (> (length S 1))
         (pre? '! S) ) )

   (de $sym (S)
      (and (sym? S)
         (> (length S 1))
         (pre? '$ S) ) )

   (de !sym-to-$sym (S)
      (any (pack '$ (cdr (chop S)))) )

   (de !syms-to-$syms (Syms)
      (mapcar !sym-to-$sym Syms) )

   (de !syms-in (L)
      (filter !sym L) )

   (de $syms-in (L)
      (uniq (filter $sym (flat L))) )

   (de mapbox (Lst)
      (mapcan ~(q `(,Q1 (box))) Lst) )

   (de letargs (Xs Ys)
      (mapcan list Xs Ys) )

)
