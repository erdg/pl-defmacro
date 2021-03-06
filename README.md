# pl-defmacro

Write Common Lisp style macros in PicoLisp for fun and profit (but mostly for fun).

```
   # PicoLisp                       # Common Lisp
   (defmacro awhen (Test . Body)    (defmacro awhen (test &rest body)
      "`"(aif ","Test                  `(aif ,Test
            (prog ",@"Body) ) )           (progn ,@Body) )
```

\**squints*\* Exactly the same!

### Testing

```
: (load "tests.l")
defmacro.l -- all tests passed
-> T
```

Hopefully that's the case on your machine too. `tests.l` also contains all the functions defined below, so you don't need to type in the definitions as you follow along.

## Usage

Let's say you're writing some Common Lisp code and you're slightly annoyed because the language doesn't offer the convenient `@`-result like PicoLisp does. No problem - thanks to the power of the Common Lisp macro system, we can easily create these abstractions.

```
### from On Lisp (p. 191)

# anaphoric 'if'
: (defmacro aif (Test Then Else)
      "`"(let it ","Test
            (if it ","Then ","Else) ) )
-> aif

: (aif (+ 1 1) (prinl "it is " it) (prinl "it is NIL"))
it is 2
-> 2

# anaphoric 'when'
: (defmacro awhen (Test . Body)
      "`"(aif ","Test
            (prog ",@"Body) ) )
-> awhen

: (awhen (+ 1 1)
   (prinl "it is " it)
   (prinl "it plus two is " (+ it 2)) )
it is 2
it plus two is 4
-> 4

# anaphoric 'and'
: (defmacro aand Args
      (cond ((not Args) T)
            ((not (cdr Args)) (car Args))
            (T "`"(aif ","(car Args) (aand ",@"(cdr Args)))) ) )
-> aand

: (aand (+ 1 2) (+ it 3) (+ it 4))
-> 10
```

Just like PicoLisp!

```
: (and (+ 1 2) (+ @ 3 (+ @ 4)))
-> 10
```

Now wouldn't it be neat if we had an anaphoric version of `list`, so we could do something like this?

```
: (alist 2 (+ it 2) (+ it 2) (+ it 2))
-> (2 4 6 8) # who do we appreciate? Common Lisp macros!
```

It sure would.

```
### from On Lisp (p. 219)

(defmacro alist Args (alist-expand Args NIL))

(de alist-expand (Args Syms)
   (if Args
      (let Sym (box)
         (quasiquote
            "`"(let [","Sym ","(car Args)  it ","Sym]
                  (alist-expand (cdr Args)
                     (append Syms (list Sym)) ) ) ) )
      (quasiquote
         "`"(list ",@"Syms) ) ) )
```

And how about an anaphoric version of `+`? You betcha!

```
### from On Lisp (p. 219)

(defmacro a+ Args (a+expand Args NIL))

(de a+expand (Args Syms)
   (if Args
      (let Sym (box)
         (quasiquote
            "`"(let [","Sym ","(car Args)  it ","Sym]
                  ","(a+expand (cdr Args)
                        (append Syms (list Sym)) ) ) ) )
      (quasiquote
         "`"(+ ",@"Syms) ) ) )
```

Notice the code duplication between the definitions of `alist` and `a+`. Wouldn't it be nice to extract this pattern so we can easily create anaphoric versions of other functions? What we need is a macro-writing macro!

```
### from On Lisp (p. 220)

# automatic anaphoric macros
(defmacro defanaph (Name)
   "`"(defmacro ","Name Args
         (anaphex Args (list (cons 'quote ","(pop-symbol Name)))) ) )

(de anaphex (Args Expr)
   (if Args
      (let Sym (box)
         (quasiquote
            "`"(let [","Sym ","(car Args)  it ","Sym]
                  ","(anaphex (cdr Args)
                        (append Expr (list Sym)) ) ) ) )
      Expr ) )

(de pop-symbol (Sym)
   (any (pack (cdr (chop Sym)))) )
```

Now we can define `alist` with a simple

```
: (defanaph alist)
# alist redefined
-> alist

# same same
: (alist 2 (+ it 2) (+ it 2) (+ it 2))
-> (2 4 6 8)
```

Any other *normal* function works too.

```
: (defanaph amapcar)
-> amapcar

# super useful!
: (amapcar +
   (list (it 1 2 3) (it 4 5 6) (it 7 8 9))
   (1 2 3) )
-> (7 17 27)

: (amapcar *
   (list (it 1 2 3) (it 4 5 6) (it 7 8 9))
   (append it (21 23 25)) )
-> (36 14400 254016)
```

This version of `defanaph` can be used on any function, so long as that function evaluates all its arguments. For example, `defanaph` may not be used to define an anaphoric `setq`, because `setq` does not evaluate its first argument. `defanaph` expects its only argument to be the name of an existing function preceeded by an "a".

## Explanation

### quasiquote

`quasiquote` does the heavy lifting for `defmacro`. It allows to use the Common Lisp backquote idiom for list interpolation within PicoLisp code.

```
: (let [X 2  L (3 4 5)]
   (quasiquote
      "`"(let Y ","X
            (+ Y ",@"L) ) ) )
-> (let Y 2 (+ Y 3 4 5)
```

`quasiquote` accomplishes this by a bunch of `macro` hacking (see source). The basic premise is to use a `macro` to transform a backquote form (see `_quasiquote`) into the standard PicoLisp `macro` idioms (`^` splicing, etc.) and pass _that_ to `macro` again for the substitutions.

So the above a `quasiquote` call is transformed to

```
(let [X 2  L (3 4 5)]
   (macro
      '(let Y ^ (list X)   # place 'X'
         (+ Y ^ L) ) )     # splice 'L'
```

which yields the expected result

```
-> (let Y 2 (+ Y 3 4 5))
```

which (if desired) can be `eval`ed.

```
: (eval @)
-> 14
```

### defmacro

Using `quasiquote` we could write a hybrid PicoLisp / Common Lisp macro like so.

```
(de aif% Lst
   (let [(Test Then Else) Lst]
      (eval
         (quasiquote
            "`"(let it ","Test
                  (if it ","Then" ","Else) ) ) ) ) )
```

And that would be fine.

```
: (aif% (+ 1 1) (+ 2 it))
-> 4
```

But after you write a couple macros in this style you realize that most of it is boilerplate code.

PicoLisp macros tend to use a variable number of unevaluated arguments (`Lst` in the example above). The `let` form destructures this list to the named parameters we are going to use in our macro. And then we `eval` a `quasiquote` with the parameters filled in.

`defmacro` simply writes the boilerplate code for us. If we look at the definition of `defmacro`, we can see the similarities to the definition of `aif%` above.

```
(de defmacro Lst
   (let [(@Nm @Args . Body) Lst]
      (macro
         (de @Nm Lst
            (let [@Args Lst]
               (eval
                  (quasiquote ^ Body) ) ) ) ) ) )
```

Everything has just been pulled up a level. `defmacro` expects the name, arguments and body of the macro we're defining and plugs them into a `de` form for us, just like the explicit version of `aif%` above.

### Conclusion
We can add new programming constructs to PicoLisp thanks to the power of the Common Lisp macro system :sunglasses:
