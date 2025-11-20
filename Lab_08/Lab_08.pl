:- use_module(library(clpfd)).

% PART A: CORE MAP COLORING PREDICATES

% Australia Map
regions_au([wa, nt, sa, q, nsw, v, t]).

edges_au([
    wa-nt, wa-sa,
    nt-sa, nt-q,
    sa-q, sa-nsw, sa-v,
    q-nsw,
    nsw-v
]).

% South America Map
regions_sa([ar, bo, br, cl, co, ec, gy, gfr, py, pe, su, uy, ve]).

edges_sa([
    ar-bo, ar-br, ar-cl, ar-py, ar-uy,
    bo-br, bo-cl, bo-py, bo-pe,
    br-co, br-gy, br-pe, br-py, br-su, br-uy, br-ve,
    cl-pe,
    co-ec, co-pe, co-ve,
    ec-pe,
    gy-gfr, gy-su, gy-ve,
    gfr-su,
    py-br, py-ar,
    pe-bo, pe-br, pe-cl, pe-co, pe-ec,
    su-br, su-gy, su-gfr,
    uy-ar, uy-br,
    ve-br, ve-co, ve-gy
]).

color_names([1-red, 2-green, 3-blue, 4-yellow, 5-purple, 6-orange, 7-pink]).

color_map(Regions, Edges, K, Vars) :-
    same_length(Regions, Vars),
    Vars ins 1..K,
    apply_edges(Regions, Vars, Edges),
    labeling([ffc], Vars).

apply_edges(Regions, Vars, Edges) :-
    maplist(constrain_edge(Regions, Vars), Edges).

constrain_edge(Regions, Vars, A-B) :-
    nth0(IndexA, Regions, A),
    nth0(IndexB, Regions, B),
    nth0(IndexA, Vars, ColorA),
    nth0(IndexB, Vars, ColorB),
    ColorA #\= ColorB.

% PART B: OPTIMIZATION


min_colors(Regions, Edges, MaxK, MinK, Vars) :-
    between(1, MaxK, K), 
    color_map(Regions, Edges, K, Vars), 
    MinK = K, 
    !.          

min_colors_au(MaxK, MinK, Vars) :-
    regions_au(Rs), 
    edges_au(Es),
    min_colors(Rs, Es, MaxK, MinK, Vars).

min_colors_sa(MaxK, MinK, Vars) :-
    regions_sa(Rs), 
    edges_sa(Es),
    min_colors(Rs, Es, MaxK, MinK, Vars).

% PART C: PRETTY PRINTING

pretty_color_by_region(Regions, Vars) :-
    color_names(ColorMap),
    maplist(print_region_color(ColorMap), Regions, Vars).

print_region_color(ColorMap, Region, ColorCode) :-
    member(ColorCode-ColorName, ColorMap),
    format('~w = ~w~n', [Region, ColorName]).

show_minimal_coloring_au :-
    regions_au(Regions),
    length(Regions, NumRegions),
    min_colors_au(NumRegions, MinK, Vars),
    format('=== AUSTRALIA MINIMAL COLORING ===~n'),
    format('Minimum colors required: ~w~n', [MinK]),
    format('Regions: ~w~n', [Regions]),
    pretty_color_by_region(Regions, Vars).

show_minimal_coloring_sa :-
    regions_sa(Regions),
    length(Regions, NumRegions),
    min_colors_sa(NumRegions, MinK, Vars),
    format('=== SOUTH AMERICA MINIMAL COLORING ===~n'),
    format('Minimum colors required: ~w~n', [MinK]),
    format('Regions: ~w~n', [Regions]),
    pretty_color_by_region(Regions, Vars).

% EXPERIMENTATION WITH LABELING STRATEGIES

color_map_strategy(Regions, Edges, K, Vars, Strategy) :-
    same_length(Regions, Vars),
    Vars ins 1..K,
    apply_edges(Regions, Vars, Edges),
    labeling(Strategy, Vars).

test_strategies :-
    writeln('Testing different labeling strategies for Australia (K=3):'),
    regions_au(Rs),
    edges_au(Es),
    Strategies = [[], [ff], [ffc], [min], [max], [leftmost], [up], [down]],
    test_strategies_with_list(Rs, Es, 3, Strategies).

test_strategies_with_list(Rs, Es, K, [Strategy|Rest]) :-
    format('Strategy ~w: ', [Strategy]),
    (   color_map_strategy(Rs, Es, K, Vars, Strategy)
    ->  writeln('SUCCESS'), writeln(Vars)
    ;   writeln('FAIL')
    ),
    test_strategies_with_list(Rs, Es, K, Rest).
test_strategies_with_list(_, _, _, []).

run_all_tests :-
    writeln('MAP COLORING OPTIMIZATION'),
    nl,
    
    show_minimal_coloring_au,
    nl,
    
    show_minimal_coloring_sa,
    nl,
    
    writeln('Additional Experiments:'),
    experiment_with_maxk,
    nl,
    
    writeln('Labeling Strategies Comparison:'),
    test_strategies.

experiment_with_maxk :-
    writeln('Testing Australia with different MaxK bounds:'),
    regions_au(Rs),
    length(Rs, NumRegions),
    between(1, NumRegions, MaxK),
    (   min_colors_au(MaxK, MinK, _)
    ->  format('MaxK=~w -> MinK=~w~n', [MaxK, MinK])
    ;   format('MaxK=~w -> No solution found~n', [MaxK])
    ),
    fail.
experiment_with_maxk.

test_au :-
    min_colors_au(4, MinK, Vars),
    regions_au(Rs),
    format('Australia - Min colors: ~w~n', [MinK]),
    pretty_color_by_region(Rs, Vars).

test_sa :-
    min_colors_sa(6, MinK, Vars),
    regions_sa(Rs),
    format('South America - Min colors: ~w~n', [MinK]),
    pretty_color_by_region(Rs, Vars).