:- use_module(library(clpfd)).
:- ['parser.pl'].

solve_griddler(Rows, Cols, Griddler, LengthX, LengthY) :-
    transpose(Griddler, GriddlerT),
    rows(Rows, Griddler, LengthY),
    rows(Cols, GriddlerT, LengthX).


%row/3, na úrovni zoznamu zoznamov
rows([], [], _).
rows([C|Cs], [R|Rs], Length) :-
    row(C, R, Length),
    rows(Cs, Rs, Length).


%najdôležitejšia metóda
%+Ks - zoznam ktorý reprezentuje riadok/stĺpec zo zadania
%-Row -  zoznam ktorý vygeneruje (vstavaná) metóda automaton/3
%+Length - očakávaná dĺžka zoznamu Row - bez pre-definovania lenght(Row, Length) sa automaton nespustí
row(Ks, Row, Length) :-
    arcs(Ks, Arcs, start, Final),
    L is Length + 1,
    length(RowZ, L),
    automaton(RowZ, [source(start), sink(Final)], [arc(start,0,start) | Arcs]), 
    %nula na konci je nutná - na konci každého riadku je n+1 stav - skok na koniec
    %keby tam tá nula nebola automaton by vrátil vrátil false
    append(Row, [0], RowZ).


%pripraví zoznam zložený z arc(from, N, to) zo vstupu
arcs([], [], Final, Final).

arcs([0|Ks], Arcs, CurState, Final) :-
    gensym(a, NextState),
    Arcs = [arc(CurState,0,CurState), arc(CurState,0,NextState) | Rest],
    arcs(Ks, Rest, NextState, Final).

arcs([K|Ks], Arcs, CurState, Final) :-
    K > 0,
    gensym(a, NextState),
    K1 is K - 1,
    Arcs = [arc(CurState,1,NextState) | Rest],
    arcs([K1|Ks], Rest, NextState, Final).


allocate_grid(Grid, X, Y) :-
    length(Grid,X),
    allocate_rows(Grid, Y).


allocate_rows([], _).
allocate_rows([R|Rs], Len) :-
    length(R, Len),
    allocate_rows(Rs, Len).


%vytlačenie jedného riadku
printit([]).
printit([R|Rs]) :-
    write('<tr style="height:5px">'),
    print_row(R),
    write('</tr>'),
    printit(Rs).


%vylačenie jednej bunky
print_row([]) :- nl.
print_row([0|R]) :-
        write('<td style="width:3px; background-color:white"</td>'),
    print_row(R).
print_row([1|R]) :-
        write('<td style="width:3px; background-color:black"></td>'),
    print_row(R).


%obmedzenie listu na zoznam zoznamov z 0,1
restrictize([]).
restrictize([Row|Rows]) :- restrict(Row), restrictize(Rows).


%obmedzenie listu na zoznam z 0,1
restrict([0]).
restrict([1]).
restrict([0|List]) :- restrict(List).
restrict([1|List]) :- restrict(List).


main(InFile) :- main(InFile, 'result.html').

main(InFile, OutFile) :-
	reset_gensym(a),
	parseFile(InFile, Rows, Cols, X, Y), 
	allocate_grid(Grid, X, Y),
	solve_griddler( Rows, Cols, Grid, X, Y),
	restrictize( Grid ),
	tell(OutFile),
	write('<html><body><table style="border-spacing:0px"><tbody>'),
	printit(Grid),
	write('</tbody></table></body></html>'),
	told,
	reset_gensym(a).