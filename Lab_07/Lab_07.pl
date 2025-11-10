:- use_module(library(clpfd)).

% Part A: Australia Map Coloring

regions_au([wa, nt, sa, q, nsw, v, t]).

edges_au([
    wa-nt, wa-sa,
    nt-sa, nt-q,
    sa-q, sa-nsw, sa-v,
    q-nsw,
    nsw-v
]).

% Color name mapping
color_names([1-red, 2-green, 3-blue, 4-yellow]).

map_color_general(Regions, Vars, Edges, K) :-
    length(Regions, N),
    length(Vars, N),
    Vars ins 1..K,
    maplist(constrain_edge(Regions, Vars), Edges).

constrain_edge(Regions, Vars, A-B) :-
    nth0(IndexA, Regions, A),
    nth0(IndexB, Regions, B),
    nth0(IndexA, Vars, ColorA),
    nth0(IndexB, Vars, ColorB),
    ColorA #\= ColorB.

colorize_au(K, Vars) :-
    regions_au(Regions),
    edges_au(Edges),
    length(Regions, N),
    length(Vars, N),
    map_color_general(Regions, Vars, Edges, K),
    labeling([ff], Vars).

pretty_color_au :-
    colorize_au(3, Vars),
    regions_au(Regions),
    color_names(ColorMap),
    writeln('AUSTRALIA MAP COLORING (3 colors)'),
    maplist(print_region_color(ColorMap), Regions, Vars).

print_region_color(ColorMap, Region, ColorCode) :-
    member(ColorCode-ColorName, ColorMap),
    format('~w = ~w~n', [Region, ColorName]).

% 4 colors version
pretty_color_au_4 :-
    colorize_au(4, Vars),
    regions_au(Regions),
    color_names(ColorMap),
    writeln('AUSTRALIA MAP COLORING (4 colors)'),
    maplist(print_region_color(ColorMap), Regions, Vars).

% Part B: South America Map Coloring

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

colorize_sa(K, Vars) :-
    regions_sa(Regions),
    edges_sa(Edges),
    length(Regions, N),
    length(Vars, N),
    map_color_general(Regions, Vars, Edges, K),
    labeling([ff], Vars).

pretty_color_sa(K) :-
    colorize_sa(K, Vars),
    regions_sa(Regions),
    color_names(ColorMap),
    format('SOUTH AMERICA MAP COLORING (~w colors)~n', [K]),
    maplist(print_region_color(ColorMap), Regions, Vars).

test_all :-
    pretty_color_au, nl,
    pretty_color_au_4, nl,
    pretty_color_sa(3), nl,
    pretty_color_sa(4).




% South America requires 4 colors (it is not 3-colorable). It contains K4 subgraphs and has high connectivity,
% which creates inevitable conflicts with only 3 colors.
