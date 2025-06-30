zombie(juan).

seInfectan(X,Y) :- zombie(X).

natural(cero).
natural(suc(X)) :- natural(X).

mayorA2(suc(suc(suc(X)))) :- natural(X). % Si le saco 3 veces suc lo que tengo sigue siendo un natural

esPar(suc(suc(cero))). % Caso base: 2 es par
esPar(suc(suc(X))) :- esPar(X). % Si parto de un par y le sumo 2, tengo otro par

menor(cero, suc(_)). 
menor(suc(X), suc(Y)) :- menor(X, Y) % Si puedo ir sacandole sucs a X y llego a cero mientras que Y es de la forma suc(_) entonces x < y 

entre(X, Y, Z) :- X =< Y, Z = X.
entre(X, Y, Z) :- X < Y, N is X+1, entre(N, Y, Z).