Génération de code
==================

On a créé un DSL pour écrire de l'assembleur. Il utilise la monade State pour
stocker un nombre qui permet de générer des labels uniques, une liste de String
qui permet de générer des labels qui ont du sens, et une Rope qui contient le
code assembleur généré.

Concernant la génération de code, on a essayé de faire un en sorte qu'il soit un
peu optimisé, c'est-à-dire qu'on a essayé qu'il ne fasse pas trop de suite
d'opérations qu'on peut simplifier de manière évidente (deux add à la suite, etc)

Concernant la fonction de génération d'expression, on n'a pas pris la convention
que le résultat est stocké sur le haut de la pile, mais on a pris la convention
qu'il soit stocké dans %rax. Si le type de l'expression est un record, alors
%rax contient un pointeur vers ce record.

Pour la valeur renvoyée par les fonctions, on a décidé de la stocker sur la pile :
on réserve un emplacement pour elle à l'appel de la fonction.

Le passage d'argument se fait sur la pile, avec, lors d'un appel, une pile de la
forme :
```
argument n
...
argument 1
%rbp father
return address
%rbp of caller
local variables...
```

Le compilateur accepte un flag --O-1 qui permet de dé-optimiser le code, si on
trouve que le code généré est trop rapide.

Il accepte aussi un flag --O1 qui permet de générer un code très rapide, en
O(taille de la sortie)
