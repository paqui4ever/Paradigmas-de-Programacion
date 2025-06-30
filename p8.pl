padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

abuelo(X, Y) :- padre(X,Z), padre(Z, Y).

% hijo(?X, ?Y)
hijo(X,Y) :- padre(Y, X).

% hermano(?X, ?Y)
hermano(X,Y) :- X \= Y, padre(P, X), padre(P, Y).

% descendiente(?X, ?Y)
descendiente(X,Y) :- X \= Y, padre(Y, X); abuelo(Y, X).

ancestro(X,X).
ancestro(X, Y) :- ancestro(Z,Y), padre(X,Z).

ancestroC(X,X).
ancestroC(X,Y) :- padre(X, Z), ancestro(Z, Y). % Reordenamos asi Z se instancia antes de llamar a descendiente

natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(0,X) :- natural(X). 
menorOIgual(suc(X), suc(Y)) :- X \= Y, menorOIgual (X, Y). % Poniendole suc en ambos argumentos puede ir hacia adelante

% juntar(?Lista1, ?Lista2, ?Lista3)
juntar([], XS, XS). % Caso base con el que se junta todo YS al resultado
juntar([X | XS], YS, [X | ZS]) :- juntar(XS, YS, ZS).

juntarAlReves(XS, [], XS).
juntarAlReves(XS, [Y | YS], [Y | ZS]) :- juntarAlReves(XS, YS, ZS).

% ultimo(?L, ?U)
ultimo([X], X).
ultimo([_ | XS], U) :- ultimo(XS, U).

% invertir(+L, ?R)
invertir([X], [X]).
invertir([X | XS], ZS) :- invertir(XS, ZS2), append(ZS2, [X], ZS). 
% Sigo invirtiendo el resto de la lista y le agrego al resultado la head de la original

% prefijo(?P, +L)
prefijo([], _).
prefijo([Y | YS], [X | XS]) :- X = Y, prefijo(YS, XS).
% hay que usar = para que unifique

prefijo2([], _).
prefijo2(XS, YS) :- append(XS, _, YS).

% sufijo(?S, +L)
sufijo(XS, XS).
sufijo(YS, [_ | XS]) :- XS = YS; sufijo(YS, XS).

sufijo2(XS,XS).
sufijo2(YS, XS) :- append(_, YS, XS).

% sublista(?S, +L)
sublista(XS, XS).
sublista(XS, [Y | YS]) :- prefijo(XS, YS).  

sublista2(XS, XS).
sublista2(XS, YS) :- sufijo(ZS, YS), prefijo(ZS, YS).

% pertenece(?X, +L)
pertenece(X, [X]).
pertenece(X, [Y | YS]) :- X = Y; pertenece(X, YS).

% aplanar(+XS, -YS)
aplanar([], []).
aplanar([XS|XSS], YS):-
    is_list(XS),
    aplanar(XS, A),
    aplanar(XSS, B),
    append(A, B, YS).
aplanar([XS|XSS], YS):-
    not(is_list(XS)),
    aplanar(XSS, B),
    YS = [XS|B].

% interseccion(+L1, +L2, -L3)
interseccion([], _, []).
interseccion([X | L1], L2, [X | L3]) :- member(X, L2), interseccion(L1, L2, L3).
interseccion([X | L1], L2, L3) :- not(member(X, L2)), interseccion(L1, L2, L3).

% partir(+N, +L, -L1, -L2)
partir(0, L2, [], L2).
partir(N, [X | L], [X | L1], L2) :- N1 is N-1, partir(N1, L, L1, L2).

% borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([], _, []).
borrar([Y | LO], X, YS) :- Y = X, borrar(LO, X, YS).
borrar([Y | LO], X, [Y | YS]) :- Y \= X, borrar(LO, X, YS).

% sacarDuplicados(+L1, -L2)
sacarDuplicados([], []).
sacarDuplicados([X | L1], L2) :- member(X, L1), sacarDuplicados(L1, L2).
sacarDuplicados([X | L1], [X | L2]) :- not(member(X, L1)), sacarDuplicados(L1, L2).

% permutacion(+L1, ?L2)
% no, gracias, estoy bien

% reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto(XS, N, [A | LL]) :- length([A | LL], N), append(A, B, XS), N1 is N-1, reparto(B, N1, LL).  

% repartoSinVacias(+L, -LListas)
repartoSinVacias([], []).
repartoSinVacias(L, [A | LL]) :- append(A, B, L), length(A, N1), N1 \= 0, repartoSinVacias(B, LL).

% desde(+X, ?Y), Y > X sino se rompe, Y si no se instancia es gen inf, si se instancia dice si X < Y.
desde(X,X).
desde(X, Y) :- N is X+1, desde(N, Y).

desdeReversible(X, Y) :- var(Y), desde(X, Y).
desdeReversible(X, Y) :- nonvar(Y), Y >= X.

nil().
bin(Izq, V, Der).

vacio(nil()).

raiz(bin(_, V, _), R) :- R = V.

altura(nil(), 0).
altura(bin(Izq, V, Der), A) :- A1 is A+1, altura(Izq, A1) > altura(Der, A1).
altura(bin(Izq, V, Der), A) :- A1 is A+1, altura(Der, A1) =< altura(Der, A1).

altura2(nil(), 0).
altura2(bin(Izq, V, Der), A) :- altura2(Izq, A1), altura2(Der, A2), A is max(A1, A2) + 1.

% inorder(+AB, -Lista)
inorder(nil(), []).
inorder(bin(Izq, V, Der), L) :- inorder(Izq, A), inorder(Der, B), append(A, [V], L1), append(L1, B, L).

% arbolConInorder(+Lista, -AB)
arbolConInorder([], nil()).
arbolConInorder(L, bin(Izq, V, Der)) :- append(A, [V | B], L), arbolConInorder(A, Izq), arbolConInorder(B, Der).

% esABB(+T)
esABB(bin(nil(), V, nil())).
esABB(bin(Izq, V, Der)) :- raiz(Izq, V1), raiz(Der, V2), V1 < V2, esABB(Izq), esABB(Der).

% ABBInsertar(+X, +T1, -T2)
ABBInsertar(X, nil(), bin(nil(), X, nil())).
ABBInsertar(X, bin(Izq1, V1, Der1), bin(Izq2, V1, Der2)) :- X < V1, ABBInsertar(X, Izq1, Izq2).
ABBInsertar(X, bin(Izq1, V1, Der1), bin(Izq2, V1, Der2)) :- X > V1, ABBInsertar(X, Der1, Der2).

% rama(+A, -C)
rama(bin(nil(), V, nil()), [V]).
rama(bin(Izq, V, Der), [V | L]) :- rama(Izq, L).
rama(bin(Izq, V, Der) [V | L]) :- rama(Der, L).