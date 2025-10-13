:- discontiguous edge/2.

% ---------- Part 1: Basic Graph ----------

edge(a, b).
edge(b, c).
edge(a, d).
edge(d, c).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).



% ---------- Part 2: Handling cycles ----------
edge(c, a).

path(X, Y) :- path(X, Y, []).

path(X, Y, _) :-
    edge(X, Y).

path(X, Y, Visited) :-
    edge(X, Z),
    \+ member(Z, Visited),
    path(Z, Y, [X|Visited]).



% ---------- Part 3: Listing all paths ----------

all_paths(X, Y, Paths) :-
	findall(P, find_path(X, Y, [X], P), Paths).

find_path(X, Y, Visited, Path) :-
	edge(X, Y),
	reverse([Y|Visited], Path).

find_path(X, Y, Visited, Path) :-
	edge(X, Z),
	\+ member(Z, Visited),
	find_path(Z, Y, [Z|Visited], Path).



% ---------- Part 4: Student Extension ----------

door(entrance, room1).
door(room1, room2).
door(room2, room3).
door(room1, room4).
door(room4, exit).

maze_path(X, Y, Path) :-
    findall(P, find_maze_path(X, Y, [X], P), AllPaths),
    member(Path, AllPaths).

find_maze_path(X, Y, Visited, Path) :-
    door(X, Y),
    reverse([Y|Visited], Path).

find_maze_path(X, Y, Visited, Path) :-
    door(X, Z),
    \+ member(Z, Visited),
    find_maze_path(Z, Y, [Z|Visited], Path).



