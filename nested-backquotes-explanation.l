#  a mini-lecture-rant on nested backquotes:

#     nested backquotes seem unnecessarily shrouded in mystery in common lisp
#     lore. as far as i can tell, they simply allow for this kind
#     of pattern:

#        you're writing a macro (the parent) that writes a macro (the child)
#        and you need to refer to the (future) run-time arguments of the child
#        macro right now (in the definition of the parent).

#     look at 'defunits.m.l'. 'defunits' is a macro used to define
#     relationships between similar types of quantities (e.g. quantities of
#     time - seconds, minutes, days, milliseconds, etc.) it does this by
#     writing another macro (named 'unit-of-[quantity]') that captures the
#     user-defined relationships as basic multiplication (using a 'case'
#     statement). that's a fancy way of saying there are 60 seconds in a minute
#     and in order to find out how many seconds are in 17 minutes we need to
#     multiply (* 17 60).

#     but right now we're righting a macro that defines another macro which
#     defines (simple multiplication) relationships between as-of-now unknown
#     quantities. but we know that whatever they are, we want to multiply them
#     to get to some number of some base unit. so we use nested backquotes (and
#     "stacked" unquotes) to name these abstract somethings and write code with
#     them now.

#     the other included example of nested backquotes is 'mac!'. 'mac!' is a
#     macro that writes macros that evaluate certain run-time arguments (those
#     prefixed with a bang '!') one time only (see 'evolution-of-mac-bang.l'
#     for a more detailed explanation).

#     "when your macro-writing (parent) macro has run-time arguments (from the
#     child) to use, nested backquotes will do!"



