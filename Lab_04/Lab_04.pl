% ----------- 1: Maze representation ------------
edge(entrance, a).
edge(a, b).
edge(a, c).
edge(b, exit).
edge(c, b).

blocked(a, c).  

% ----------- 2: Reasoning rules	 -------------------
can_move(X, Y) :-
    edge(X, Y),
    \+ blocked(X, Y).

reason(X, Y, 'path is open') :-
    can_move(X, Y).

reason(X, Y, 'path is blocked') :-
    blocked(X, Y).

reason(_, Y, 'destination reached') :-
    Y == exit.

% ----------- 3: Recursive traversal ----------------------
move(X, Y, Visited, [Y|Visited]) :-
    can_move(X, Y),
    format('Moving from ~w to ~w.~n', [X, Y]),
    reason(X, Y, R),
    format('Reason: ~w.~n', [R]).

move(X, Y, Visited, Path) :-
    can_move(X, Z),
    \+ member(Z, Visited),
    format('Exploring from ~w to ~w...~n', [X, Z]),
    reason(X, Z, R),
    format('Reason: ~w.~n', [R]),
    move(Z, Y, [Z|Visited], Path).

% ----------- 4: Main Predicate ----------------------
find_path(Start, Goal, Path) :-
    move(Start, Goal, [Start], RevPath),
    reverse(RevPath, Path),
    format('~nFinal Path: ~w~n', [Path]).

% ----------- 5: Extension -------------------

why(X, Y) :-
    reason(X, Y, Explanation),
    format('Reasoning from ~w to ~w because the ~w.~n', [X, Y, Explanation]).

/*
This program implements an intelligent system that navigates 
through a maze represented as a graph. It uses facts, rules, 
recursion, and symbolic reasoning to find a path from an 
entrance to an exit, explaining each step.
*/


