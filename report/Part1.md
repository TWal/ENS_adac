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

Pour le typeur, la principale difficulté a été de bien définir les
structures utilisées. Après quelques expérimentation, on utilise
coome contexte une structure qui contient une map pour les fonctions,
une pour les variables, et une pour les nouveux types. Une quatrième
map a été ajoutée pour indiquer que des identifieurs ont étés réservés,
et qu'ils ne peuvent pas désigner de types.

Afin de gérer le masquage, l'environnement est une pile de contextes.

Afin de gérer les mises à jour de l'environnement en Haskell, toutes
les fonctions de typage sont encapsulée dans un monad `StateT`, ce
qui permet en plus, en l'inscrivant sur un monad `Either String`, de
lancer des exeptions sous la forme de texte, pour gérer les erreurs.

Le format de l'environnement n'étant pas évident à manipuler correctement,
on a implémenté des fonctions utilitaires d'abord.

Créer l'AST typé s'est fait en reprenant l'AST du parseur avec peu de 
modifications.

Le typage a ensuite été relativement simple à faire, la structure du
programme collant de près à la structure de l'AST du parseur.

