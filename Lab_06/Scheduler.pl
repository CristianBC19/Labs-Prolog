:- use_module(library(clpfd)).

% Part 1 – Setup ----------------------------
task(t1, 4, 1).
task(t2, 3, 2).
task(t3, 5, 1).
task(t4, 2, 3).
task(t5, 3, 2).

% Part 2 – Core Logic ------------------------
schedule(Tasks, Starts, Ends, Makespan) :-
    findall(task(N,D,R), task(N,D,R), Tasks),

    length(Tasks, N),
    length(Starts, N),
    length(Ends, N),

    Starts ins 0..30,
    Ends   ins 0..40,

    findall(D, task(_,D,_), Durations),
    findall(R, task(_,_,R), Resources),

    maplist(constrain_task, Starts, Durations, Ends),
    apply_non_overlap(Starts, Durations, Resources),

    nth0(0, Ends, Ea),
    nth0(1, Starts, Sb),
    Ea #=< Sb,

    Makespan in 0..40,
    max_constraint(Ends, Makespan),

    append(Starts, [Makespan], Vars),
    labeling([min(Makespan)], Vars).

constrain_task(S, D, E) :- E #= S + D.

apply_non_overlap([], [], []).
apply_non_overlap([S|Ss], [D|Ds], [R|Rs]) :-
    no_overlap_with_others(S, D, R, Ss, Ds, Rs),
    apply_non_overlap(Ss, Ds, Rs).

no_overlap_with_others(_, _, _, [], [], []).
no_overlap_with_others(S1, D1, R1, [S2|Ss], [D2|Ds], [R2|Rs]) :-
    (R1 #= R2 ->
        S1 + D1 #=< S2 #\/ S2 + D2 #=< S1
    ; true),
    no_overlap_with_others(S1, D1, R1, Ss, Ds, Rs).

max_constraint([], _).
max_constraint([E|Es], Max) :-
    E #=< Max,
    max_constraint(Es, Max),
    member(Max, [E|Es]).

% Part 3 – Output -----------------------------
run_schedule :-
    schedule(Tasks, Starts, Ends, Makespan),
    format('--- Schedule ---~n'),
    print_schedule(Tasks, Starts, Ends),
    format('makespan=~w~n',[Makespan]).

print_schedule([], [], []).
print_schedule([task(N,D,R)|Ts],[S|Ss],[E|Es]):-
    format('~w [res=~w, dur=~w]\t start=~w\t end=~w~n',[N,R,D,S,E]),
    print_schedule(Ts,Ss,Es).
