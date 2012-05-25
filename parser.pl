parseFile(File, MazeX, MazeY, SizeX, SizeY) :-
	process(File, Out),
	flattern(Out, [[SizeX, SizeY]|Maze]),
	append(MazeX, MazeY, Maze),
	length(MazeX, SizeX),
	length(MazeY, SizeY).

process(File, Out) :-
        open(File, read, In),
        get_char(In, Char1),
        process_stream(Char1, In, Out, nonnumeric),
        close(In).

flattern([i, newline], [[]]) :- !.
flattern([], [[]]).
flattern([X], [[X]]).

flattern([newline,i|Tail1], [[]|Tail2]) :-
	flattern( Tail1, Tail2), !.

flattern([i,newline,i|Tail1], [[]|Tail2]) :-
	flattern( Tail1, Tail2), !.

flattern([i|Tail1], [[i|A2]|Tail2]) :-
	flattern( Tail1, [A2|Tail2]), !.

flattern([A1|Tail1], [[Res|Tail]|Tail2]) :-
	integer(A1),
	flattern( Tail1, [[i,A2|Tail]|Tail2]),
	integer(A2),
	Res is ((10*A1) + A2), !.

flattern([A1|Tail1], [[A1|A2]|Tail2]) :-
	integer(A1),
	flattern( Tail1, [A2|Tail2]).




process_stream(end_of_file, _, [], _) :- !.
process_stream(Char, In, Nums, numeric) :-
	\+num(Char, _),
	get_char(In, Char2),
        process_stream(Char2, In, Nums, nonnumeric).

process_stream(Char, In, [i,Num|Nums], numeric) :-
	num(Char, Num), 
	get_char(In, Char2),
        process_stream(Char2, In, Nums, numeric).

process_stream(Char, In, Nums, nonnumeric) :-
	\+num(Char, _),
	get_char(In, Char2),
        process_stream(Char2, In, Nums, nonnumeric).

process_stream(Char, In, [Num|Nums], nonnumeric) :-
	num(Char, Num), 
	get_char(In, Char2),
        process_stream(Char2, In, Nums, numeric).

num('\n', newline).
num('0', 0).
num('1', 1).
num('2', 2).
num('3', 3).
num('4', 4).
num('5', 5).
num('6', 6).
num('7', 7).
num('8', 8).
num('9', 9).
