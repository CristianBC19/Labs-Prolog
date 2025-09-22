% --------------------------------------------------------
parent(patricia, cristian).
parent(patricia, sari).
parent(patricia, leonel).
parent(patricia, sebastian).
parent(marcelo, cristian).
parent(marcelo, sari).
parent(marcelo, leonel).
parent(marcelo, sebastian).
parent(gilberto, patricia).
parent(paulina, patricia).


% --------------------------------------------------------
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% --------------------------------------------------------

likes(cristian, mixto).
likes(patricia, cuy).
likes(sari, asado).
likes(cristian, sushi).
likes(marcelo, sushi).
likes(sebastian, hornado).
likes(paulina, cuy).
likes(gilberto, coladamorada)

% --------------------------------------------------------

food_friend(X, Y) :-
    likes(X, Food),
    likes(Y, Food),
    X \= Y.

% --------------------------------------------------------

factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% --------------------------------------------------------

sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

% --------------------------------------------------------

length_list([], 0).
length_list([_|T], L) :-
    length_list(T, L1),
    L is L1 + 1.

% --------------------------------------------------------

append_list([], L, L).
append_list([H|T], L, [H|R]) :-
    append_list(T, L, R).

% Example Queries to Run

% 14. ?- ancestor(patricia, X).
% 15. ?- sibling(sari, X).
% 16. ?- food_friend(X, Y).
% 17. ?- factorial(6, F).
% 18. ?- sum_list([2,4,6,8], S).
% 19. ?- length_list([a,b,c,d], L).
% 20. ?- append_list([1,2], [3,4], R).
