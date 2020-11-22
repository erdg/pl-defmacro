# this file is for documentation only and will not work
(m
   
   ### mac$ (defmacro/g! from LOL
   #
   # this is good
   (de mac$1 Lst
      (let [(Name Args . Body) Lst]
         (let [$Syms (uniq (filter $sym (flat Body)))]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst  ,(mapcan '((S) (quasi `(,S (box)))) $Syms)]
                     (evq
                        ,@Body ) ) ) ) ) ) )

   ### - mac!
   #
   # this kinda works but is totally broken on the inside. it doesn't quite
   # generate the right expansion. it passes simple tests, but does not work
   # when there is more than one "!" symbol in the form.

   (de mac!1 Lst
      (let [(Name Args . Body) Lst]
         (let [!s (filter !sym Args)  $s (mapcar !sym-to-$sym !s)]
            (evq
               `(mac$ ,Name ,Args
                  '(let ,,(mapcan list (list ,@$s) (list ,@!s))
                     ,@Body) ) ) ) ) )

   # so i decided to scrap the idea of the 'mac!' calling 'mac$' (as in LOL)
   # and included the definition of 'mac$' in the definition of 'mac!'. this
   # works as expected.
   
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
   # which resulted in the creation of a read-macro 'enq' to make writing
   # "nested" backquotes look cooler (and like they kinda worked). but it's a
   # hack nested - backquotes don't really work, because backquote doesn't
   # really do anything in this system besides add a literal 'quote to a list. 

   # NOTE -this version has NO implicit backquote in the body, so macros are
   # defined as
   #
   #  (mac! square (!X) `(* ,$X ,$X) )

   (de mac!3 Lst
      (let [(Name Args . Body) Lst]
         (let [!Syms (filter !sym Args)  $Syms (mapcar !sym-to-$sym !Syms)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,(mapcan '((S) (quasi `(,S (box)))) (uniq (filter $sym (flat Body))))
                        ~(enq
                           `(let ,,(mapcan list (list ,@$Syms) (list ,@!Syms))
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )

   # this version has an implicit backquote in the body, so macros are defined
   # as
   #
   #  (maq! square (!X) (* ,$X ,$X) )
   #
   # it's called maq! (with a 'q') because of the implied *q*uasiqoute

   (de maq! Lst
      (let [(Name Args . Body) Lst]
         (let [!Syms (filter !sym Args)  $Syms (mapcar !sym-to-$sym !Syms)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,(mapcan '((S) (quasi `(,S (box)))) (uniq (filter $sym (flat Body))))
                        ~(enq
                           `(let ,,(mapcan list (list ,@$Syms) (list ,@!Syms))
                              ,@Body ) ) ) ) ) ) ) ) )


   # now i'm thinking about read-macros. how do i execute code at read-time
   # after shadowing the backquote read-macro for these PL extensions?
   # fortunately, the tilde '~' read-macro is still available. who needs two
   # read-macros anyway?  if you've got one that can splice, you've got one
   # that can place (by writing functions that 'list' their results.

   # the 'qfn' read-macro is just that. its the sharp-backquote of this PL
   # macro lang. 'qfn' accepts 'Qsym' anaphors (Q1 ... QN) and returns a
   # function with things spliced and diced in.

   # ~(qfn `(,Q1 (box))) -> '((Q1) (quasiquote "`"(","Q1 (box))))

   (de mac!4 Lst
      (let [(Name Args . Body) Lst]
         (let [!Args (filter !sym Args) $Args (mapcar !sym-to-$sym !Args)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,[mapcan ~(qfn `(,Q1 (box))) (uniq (filter $sym (flat Body)]
                        ~(enq
                           `(let ,,[mapcan list (list ,@$Args) (list ,@!Args)]
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )

   # and then i wrote a bunch of small utility functions to hide the mapping
   # and filtering and provide more descriptive names for the underlying
   # processes. the form of the original is clearly preserved, but this version
   # is tight. it's less than 80 char wide, and it reads like english.

   # mac! is a macro-writing macro.
   #
   #  (mac! square (!X)    # args prefixed with '!' are evaluated once only
   #     `(* ,$X ,$X) )    # and referred to as prefixed with '$'
   #
   # this prevents multiple evaluation of !arguments (which can produce
   # unexpected results when side-effects are involved.
   #
   #  : (let Cnt 2 (square (inc 'Cnt)) -> 9
   #
   #  (mac square-buggy (X) `(* ,X ,X))
   #
   #  : (let Cnt 2 (square-buggy (inc 'Cnt)))   # (inc 'Cnt) evaluated twice
   #  ->12
   #
   # 'mac!' does this by gathering the !args and creating the corresponding
   # $syms.  it then searches the body of the macro being defined for any
   # additional $syms.  all the $syms are then bound to 'box'es in the
   # expansion. finally - when the defined macro is called (hence the nested
   # backquotes) - the result of evaluting the !args provided to the are bound
   # to the $sym 'box'es.
   #
   # so 'mac!' creates a bunch of variable bindings in the macro it defines
   # (some of which are are finally filled in during *that* macro's runtime) to
   # provide us with a convenient way of handling variable capture and
   # execution when writing macros.
   #
   # the symbol '$' was chosen because the function 'box' returns anonymous
   # transient symbols (picolisp gensyms) as e.g.  '$1723986599283'

   (de mac! Lst
      (let [(Name Args . Body) Lst]
         (let [!Args (!syms-in Args)  $Args (!syms-to-$syms !Args)]
            (evq
               `(de ,Name Lst
                  (let [,Args Lst]
                     (let ,[mapbox ($syms-in Body)]
                        ~(enq
                           `(let ,,[letargs (list ,@$Args) (list ,@!Args)]
                              (evq ,@Body ) ) ) ) ) ) ) ) ) )


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

   (de mapboxes (Lst)
      (mapcan ~(q `(,Q1 (box))) Lst) )

   (de letargs (Xs Ys)
      (mapcan list Xs Ys) )

)
