nil.
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
rama(bin(nil, V, nil), [V]).
rama(bin(Izq, V, Der), [V | L]) :- rama(Izq, L).
rama(bin(Izq, V, Der), [V | L]) :- rama(Der, L).

% generaLongitudes (+A, -LI, - LD)
generaLongitudes(nil, 0, 0).
generaLongitudes(bin(Izq, V, Der), ) :- rama(Der, T1), rama(Izq, T2), length(T1, N1), length(T2, N2), LI = N2 + 1, LD = N1 + 1.

% ramaMasLarga(+A, -C)
ramaMasLarga(nil, []).
ramaMasLarga(bin(Izq, V, Der), [V | L]) :- rama(Der, T1), rama(Izq, T2), length(T1, N1), length(T2, N2), N1 > N2, L = T1.
ramaMasLarga(bin(Izq, V, Der), [V | L]) :- rama(Der, T1), rama(Izq, T2), length(T1, N1), length(T2, N2), N1 < N2, L = T2.

% hayOtraDeMismaL(+A, +N)
hayOtraDeMismaL(nil, 0).
hayOtraDeMismaL(A, N) :- rama(A, D), D \= C, length(D, N).

% ramaUnicaDeLong(+A, +N, -C)
ramaUnicaDeLong(nil, 0, []).
ramaUnicaDeLong(A, N, L) :- length(L, N), rama(A, L), not(hayOtraDeMismaL(A, N)).

% extraeHead(+L, -H)
extraeHead([], []).
extraeHead([H | T], H).

% subsecuencia(+L, -R)
subsecuencia([], []).
subsecuencia([H | T], [H | R]) :- subsecuencia(T, R).
subsecuencia([_ | T], R) :- subsecuencia(T, R) 

% creciente(+L)
creciente([]).
creciente([X]).
creciente([X |Y | XS]) :- X < Y, creciente([Y | XS]).

% subsecuenciaCreciente(+L, -S)
subsecuenciaCreciente(L, S) :- subsecuencia(L, S), creciente(S).

% hayOtraMasLarga(+L, +S)
hayOtraMasLarga(L, S) :- subsecuenciaCreciente(L, D), D \= S, length(S, N1), length(D, N2), N2 > N1. 

% subsecuenciaCrecienteMasLarga(+L, -S)
subsecuenciaCrecienteMasLarga(L, S) :- subsecuenciaCreciente(L, S), not(hayOtraMasLarga(L, S)).

% suma(+L, -S)
suma([], 0).
suma([X | XS], S) :- suma(XS, N), S is N + X.

% arbol(-A)
arbol(nil).
arbol(bin(Izq, V, Der)) :- arbol(Izq), arbol(Der).

% nodosEn(?A, +L)
nodosEn(nil, _).
nodosEn(bin(Izq, V, Der), L) :- member(V, L), nodosEn(Izq, L), nodosEn(Der, L).

% sinRepEn(-A, +L) 
sinRepEn(nil, _).
sinRepEn(bin(Izq, X, Der), L) :- length(L, N), N >= 1, select(X, L, L1), sinRepEn(Izq, LI), sinRepEn(Der, LD), append(LI, LD, L1).

% proximoNumPoderoso(X, Y) :- Y is X+1, numeroPoderoso(Y), !.
% proximoNumPoderoso(X, Y) :- N is X+1, not(numeroPoderoso(N)), proximoNumPoderoso(N, Y).

% numeroPoderoso(X) :- divisores(X, L), todosPrimosPotencia(L, L).

% % divisores(+N, -Divs)
% divisores(N, Divs) :-
%     divisores_aux(1, N, Divs).

% % divisores_aux(+D, +N, -Divs)
% divisores_aux(D, N, []) :-
%     D > N, !.
% divisores_aux(D, N, [D | Resto]) :-
%     N mod D =:= 0,
%     D1 is D + 1,
%     divisores_aux(D1, N, Resto).
% divisores_aux(D, N, Resto) :-
%     N mod D =\= 0,
%     D1 is D + 1,
%     divisores_aux(D1, N, Resto).

% todosPrimosPotencia([], _).
% todosPrimosPotencia([X | XS], L) :- primo(X), X2 is X*X, member(X2, L), todosPrimosPotencia(XS, L).  

primo(X) :- divisores(X, XS), length(XS, 2), member(X, XS), member(1, XS).

esPrimo(2).
esPrimo(N) :-
    N > 2,
    N mod 2 == 0,
    not(tiene_divisor(N, 3)).

tiene_divisor(N, D) :-
    D * D =< N,
    (N mod D =:= 0 ; D2 is D + 2, tiene_divisor(N, D2)).
esPoderoso(M) :-
    M > 0,
    \+ (between(2, M, P),
        esPrimo(P),
        M mod P =:= 0,
        P2 is P * P,
        M mod P2 == 0).  % Si algún primo divide pero su cuadrado no, falla
proximoNumPoderoso(X, Y) :-
    X1 is X + 1,
    buscarPoderosoDesde(X1, Y).

buscarPoderosoDesde(N, N) :- esPoderoso(N), !.
buscarPoderosoDesde(N, Y) :-
    N1 is N + 1,
    buscarPoderosoDesde(N1, Y).