Lexeur
=====

On a utilisé la bibliothèque Alex (le flex de Haskell).

La principale difficulté a été de comprendre comment fonctionne le système pour
utiliser un "userState" afin d'avoir le nom du fichier quand on veut afficher
un message d'erreur.

Chaque Token contient sa position. Au début on avait

    data Token = Token TokenClass Position
    data TokenClass = TokenEOF | ... -- (les tokens)

Mais finalement on a fait

    data Token = TokenEOF Position | ...

Pour que le parseur puisse facilement accéder à la position et à l'information
supplémentaire du token (comme `TokenIdent` qui contient un `String`)

Parseur
======

On a utilisé la bibliothèque Happy (le bison de Haskell).

Il n'y a pas eu de problème car il s'intègre parfaitement avec Alex.

AST
===

Au début on voulait utiliser le pattern

    data Expr e = Plus e e | Sub e e | ...
    data ExprFix = ExprFix (Expr ExprFix)

Mais ça ne se généralise pas bien quand l'AST contient plusieurs types
différents (Expr, Instr, ...)

Finalement on a fait un AST annoté où chaque type prends en argument un type
d'annotation (par exemple la position), et on a définit le type

    type Ann a b = (a b, b)

Où `a` est un type de l'arbre et `b` est une annotation.

Typeur
=====

TODO
