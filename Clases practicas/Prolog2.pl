% Ejercicio 1

% iesimo(+I, +L, -X)
iesimo(0, [X | _], X). % Si el indice es 0 siempre te da la cabeza de la lista
iesimo (I, [_ | XS], X) :- N is I-1, iesimo(N, XS, X). % Podria poner I > 0 al principio, 
                                                       % buscando que quede mas clara la disjunción de las reglas

% Ejercicio 2
% iesimoReversible(?I, +L, -X)
iesimoReversible(0, [X|_],X).
iesimoReversible(I, [_|XS], X) :- iesimoReversible(N, XS, X), I is N+1.

% Ejercicio 3
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).
% Como instanciar los parametros? X debe estar instanciada pero Y no, porque si lo estuviese seguiria infinitamente
% desde(+X, -Y)

% Ejercicio 4
% desdeReversible (+X, ?Y)
desdeReversible(X, Y) :- nonvar(Y), Y >= X. % Si no esta instanciado devuelve si Y es mayor o igual a X
desdeReversible(X, Y) :- var(Y), desde(X,Y). 

% Ejercicio 5
% pmq(+X, -Y) 
pmq(X,X) :- esPar(X).
pmq(X, X-1) :- X > 1, esImpar(X), N is X-1, pmq(N, Y). % Revisar este caso
pmq(X, Y) :- X > 0, N is X-2, pmq(N, Y).

pmq2(X,Y) :- between(0,X,Y), esPar(Y). % genero subcto de soluciones con between y checkeo las soluciones con esPar (GENERATE AND TEST)

esPar(X) :- X mod 2 =:= 0.
esImpar(X) :- X mod 2 =:= 1.

% Ejercicio 6
% coprimos(-X, -Y)
coprimos(X, Y) :- desde(1,X), desde(1,Y), gcd(X,Y) =:= 1. % Veo combinaciones entre numeros y verifico con gcd si son coprimos

% Ejercicio 7
% todosLosPares(-X, -Y)
todosLosPares(X,Y) :- desde(0, S), between(0, S, X), Y is S-X.
% Si X viene instanciado va a generar todos los S que puede
% Si Y viene instanciado va a generar X hasta llegar a un X que cumpla Y is S-X 
% Si vienen los dos instanciados se va colgar infinitamente porque va a generar S infinitamente

% Ejercicio 8
% corteMasParejo(+L,-L1,-L2)
corteMasParejo(L,L1,L2) :- unCorte(L,L1,L2,D), not(hayCorteMejor(L,D)). % Si no hay corte mejor es el minimo

hayCorteMejor(L, D) :- unCorte(L,_,_,D2), D2 < D.

% unCorte(+L, -L1, -L2, -D)
unCorte(L,L1,L2,D) :- append(L1,L2,L), sumList(L1,S1), sumList(L2,S2), D is abs(S1-S2).

% Ejercicio 9
% proximoPrimo(+N, -P)
proximoPrimo(N, P) :- P is N+1, esPrimo(P).
proximoPrimo(N, P) :- N2 is N+1, not(esPrimo(N2)), proximoPrimo(N2, P).

% esPrimo(+X)
esPrimo(X) :- X > 1, Xant is X-1, not((between(2,Xant,D), mod(X,D) =:= 0)).
