:- use_module(library(clpfd)).


% 1. Main Sudoku Solver ---------------------------------------------

sudoku(Rows) :-
    append(Rows, Vars), Vars ins 1..9,
    maplist(all_different, Rows),
    transpose(Rows, Columns), maplist(all_different, Columns),
    blocks(Rows),
    maplist(label, Rows).

% 2. Block Constraints ----------------------------------------------

blocks([]).
blocks([A, B, C | Rest]) :-
    blocks3(A, B, C),
    blocks(Rest).

blocks3([], [], []).
blocks3([A1, A2, A3 | R1],
        [B1, B2, B3 | R2],
        [C1, C2, C3 | R3]) :-
    all_different([A1, A2, A3, B1, B2, B3, C1, C2, C3]),
    blocks3(R1, R2, R3).

% 3. Test and Extend ------------------------------------------------

% 3.1 Example Puzzle (0 = blank cell)

example([
 [5,3,0, 0,7,0, 0,0,0],
 [6,0,0, 1,9,5, 0,0,0],
 [0,9,8, 0,0,0, 0,6,0],

 [8,0,0, 0,6,0, 0,0,3],
 [4,0,0, 8,0,3, 0,0,1],
 [7,0,0, 0,2,0, 0,0,6],

 [0,6,0, 0,0,0, 2,8,0],
 [0,0,0, 4,1,9, 0,0,5],
 [0,0,0, 0,8,0, 0,7,9]
]).

% 3.2 Formatted Grid Output -----------------------------------------

pretty_print(Rows) :-
    print_separator,
    print_rows(Rows, 0).

print_rows([], _).
print_rows([Row|Rest], Count) :-
    print_row(Row),
    NewCount #= Count + 1,
    (   NewCount mod 3 =:= 0
    ->  print_separator
    ;   true
    ),
    print_rows(Rest, NewCount).

print_row([A1,A2,A3,A4,A5,A6,A7,A8,A9]) :-
    format('| ~w ~w ~w | ~w ~w ~w | ~w ~w ~w |~n',
           [A1,A2,A3, A4,A5,A6, A7,A8,A9]).

print_separator :-
    writeln('+-------+-------+-------+').

% 3.3 Convert Puzzle with Zeros to Variables -------------------------

puzzle_from_list(P0, P) :-
    maplist(convert_row, P0, P).

convert_row(Row0, Row) :-
    maplist(convert_cell, Row0, Row).

convert_cell(0, _) :- !.   % 0 means an empty cell (variable)
convert_cell(N, N).

% 3.4 Solver with Display -------------------------------------------

solve_and_print(P0) :-
    puzzle_from_list(P0, P),
    sudoku(P),
    writeln('Solved Sudoku:'),
    pretty_print(P).

% 3.5 Optional (Board Validation) -----------------------------------

valid_board(Rows) :-
    length(Rows, 9),
    maplist(valid_row, Rows),
    transpose(Rows, Cols),
    maplist(no_conflicts, Rows),
    maplist(no_conflicts, Cols),
    blocks_no_conflicts(Rows).

valid_row(Row) :-
    length(Row, 9).

no_conflicts(Row) :-
    include(number, Row, Fixed),
    all_different(Fixed).

blocks_no_conflicts([]).
blocks_no_conflicts([A,B,C|Rest]) :-
    block_conflict(A,B,C),
    blocks_no_conflicts(Rest).

block_conflict([], [], []).
block_conflict([A1,A2,A3|R1], [B1,B2,B3|R2], [C1,C2,C3|R3]) :-
    include(number, [A1,A2,A3,B1,B2,B3,C1,C2,C3], Fixed),
    all_different(Fixed),
    block_conflict(R1,R2,R3).

% 3.6 Optional (Uniqueness Check) -----------------------------------

unique_solution(Puzzle) :-
    findall(Puzzle, sudoku(Puzzle), Solutions),
    length(Solutions, Count),
    Count =:= 1.
