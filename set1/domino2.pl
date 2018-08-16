dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
(2,2),(2,3),(2,4),(2,5),(2,6),
(3,3),(3,4),(3,5),(3,6),
(4,4),(4,5),(4,6),
(5,5),(5,6),
(6,6)]).

frame([[3,1,2,6,6,1,2,2],
[3,4,1,5,3,0,3,6],
[5,6,6,1,2,4,5,0],
[5,6,4,1,3,3,0,0],
[6,1,0,6,3,2,4,0],
[4,1,5,2,4,3,5,5],
[4,1,0,2,4,5,2,0]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
domino_in_line(_,_,[[]],_, []).
domino_in_line(_,_,[[],[]],_, []).

domino_in_line(LineNo, RowNo, [[X,Y|T1]], (X,Y), [(X,Y,LineNo,RowNo,LineNo,NextRow)|Tail]):- NextRow is RowNo + 1,
																			domino_in_line(LineNo, NextRow, [[Y|T1]], (X,Y), Tail).

domino_in_line(LineNo, RowNo, [[X,Y|T1], [Y|T2]], (X,Y), [(X,Y,LineNo,RowNo,NextLine,RowNo), (X,Y,LineNo,RowNo,LineNo,NextRow)|Tail]):-	NextRow is RowNo + 1,
                                          NextLine is LineNo + 1,
																					domino_in_line(LineNo, NextRow, [[Y|T1], T2], (X,Y), Tail).

domino_in_line(LineNo, RowNo, [[X,Y|T1], [_|T2]], (X,Y), [(X,Y,LineNo,RowNo,LineNo,NextRow)|Tail]):-	NextRow is RowNo + 1,
																						domino_in_line(LineNo, NextRow, [[Y|T1], T2], (X,Y), Tail).

domino_in_line(LineNo, RowNo, [[X|T1], [Y|T2]], (X,Y), [(X,Y,LineNo,RowNo,NextLine,RowNo)|Tail]):-	NextRow is RowNo + 1,
                                          NextLine is LineNo + 1,
																					domino_in_line(LineNo, NextRow, [T1, T2], (X,Y), Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domino_in_line(LineNo, RowNo, [[X,Y|T1]], (Y,X), [(X,Y,LineNo,RowNo,LineNo,NextRow)|Tail]):- NextRow is RowNo + 1,
																			domino_in_line(LineNo, NextRow, [[Y|T1]], (X,Y), Tail).

domino_in_line(LineNo, RowNo, [[X,Y|T1], [Y|T2]], (Y,X), [(X,Y,LineNo,RowNo,NextLine,RowNo), (X,Y,LineNo,RowNo,LineNo,NextRow)|Tail]):-	NextRow is RowNo + 1,
																					NextLine is LineNo + 1,
                                          domino_in_line(LineNo, NextRow, [[Y|T1], T2], (Y,X), Tail).

domino_in_line(LineNo, RowNo, [[X,Y|T1], [_|T2]], (Y,X), [(X,Y,LineNo,RowNo,LineNo,NextRow)|Tail]):-	NextRow is RowNo + 1,
																						domino_in_line(LineNo, NextRow, [[Y|T1], T2], (Y,X), Tail).

domino_in_line(LineNo, RowNo, [[X|T1], [Y|T2]], (Y,X), [(X,Y,LineNo,RowNo,NextLine,RowNo)|Tail]):-	NextRow is RowNo + 1,
                                          NextLine is LineNo + 1,
																					domino_in_line(LineNo, NextRow, [T1, T2], (Y,X), Tail).


domino_in_line(LineNo, RowNo, [[_|T1], [_|T2]], (Y,X), Tail):- 	NextRow is RowNo + 1,
																domino_in_line(LineNo, NextRow, [T1, T2], (Y,X), Tail).																					
domino_in_line(LineNo, RowNo, [[_|T1]], (Y,X), Tail):- 	NextRow is RowNo + 1,
														domino_in_line(LineNo, NextRow, [T1], (Y,X), Tail).	

domino_in_line(_,_,[[_],[_]],_, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
domino_in_grid(_, _, [], []).

domino_in_grid(LineNo, (X,Y), [GridLine1,GridLine2|RestLines], Result):- 	domino_in_line(LineNo, 0, [GridLine1,GridLine2], (X,Y), Result1),
																			NextLine is LineNo + 1,
																			domino_in_grid(NextLine, (X,Y), [GridLine2|RestLines], Tail),
																			append(Result1, Tail, Result).

domino_in_grid(LineNo, (X,Y), [GridLine1], Result):- domino_in_line(LineNo, 0, [GridLine1], (X,Y), Result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dominos_positons([], _, []).
dominos_positons([Domino|Tail], Frame, [Positions|RestPositions]):- domino_in_grid(0, Domino, Frame, Positions),
                                                                    dominos_positons(Tail, Frame, RestPositions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

place_dominos([], _, []).
place_dominos([[]], _, []).
%place_dominos([[]|Dominos], Used, [[(skata)]|Solution]) :- place_dominos(Dominos, Used, Solution).
place_dominos([[(A,B,SX,SY,EX,EY)|_]|Dominos], Used, [(A,B,SX,SY,EX,EY)|Solution]) :- not(member((_,_,SX,SY,_,_), Used)),
                                                                          not(member((_,_,_,_,SX,SY), Used)),
                                                                          not(member((_,_,_,_,EX,EY), Used)),
                                                                          not(member((_,_,EX,EY,_,_), Used)),
                                                                          append(Used, [(A,B,SX,SY,EX,EY)], NewUsed) ,
                                                                          place_dominos(Dominos, NewUsed, Solution),!.

%place_dominos([[(_,_,SX,SY,_,_)|RestPos]|Dominos], Used, Solution) :- member((_,_,SX,SY,_,_), Used), place_dominos([RestPos|Dominos], Used, Solution).
%place_dominos([[(_,_,_,_,EX,EY)|RestPos]|Dominos], Used, Solution) :- member((_,_,_,_,EX,EY), Used), place_dominos([RestPos|Dominos], Used, Solution).
%place_dominos([[(_,_,_,_,EX,EY)|RestPos]|Dominos], Used, Solution) :- member((_,_,EX,EY,_,_), Used), place_dominos([RestPos|Dominos], Used, Solution).
%place_dominos([[(_,_,SX,SY,_,_)|RestPos]|Dominos], Used, Solution) :- member((_,_,_,_,SX,SY), Used), place_dominos([RestPos|Dominos], Used, Solution).
place_dominos([[_|RestPos]|Dominos], Used, Solution) :- place_dominos([RestPos|Dominos], Used, Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_line_hor(_, RowNo, _, RowLength):- RowNo >= RowLength, write('\n').
print_line_hor(LineNo, RowNo, Positions, RowLength):-  NextRow is RowNo+1, member((A,_, LineNo,RowNo,LineNo,NextRow),Positions), write(A), write('-'),print_line_hor(LineNo, NextRow, Positions, RowLength),!. 
print_line_hor(LineNo, RowNo, Positions, RowLength):-  NextRow is RowNo+1, member((A,_, LineNo,RowNo,_,_),Positions), write(A), write(' '),print_line_hor(LineNo, NextRow, Positions, RowLength),!. 
print_line_hor(LineNo, RowNo, Positions, RowLength):-  NextRow is RowNo+1, member((_,B, _,_,LineNo,RowNo),Positions), write(B), write(' '),print_line_hor(LineNo, NextRow, Positions, RowLength),!. 

print_line_ver(_, RowNo, _, RowLength):- RowNo >= RowLength, write('\n').
print_line_ver(LineNo, RowNo, Positions, RowLength):-  NextRow is RowNo+1, NextLine is LineNo+1, member((_,_, LineNo,RowNo,NextLine,RowNo),Positions), write('|  '),print_line_ver(LineNo, NextRow, Positions, RowLength),!. 
print_line_ver(LineNo, RowNo, Positions, RowLength):-  NextRow is RowNo+1, member((_,_, LineNo,RowNo,_,_),Positions), write('   '),print_line_ver(LineNo, NextRow, Positions, RowLength),!. 
print_line_ver(LineNo, RowNo, Positions, RowLength):-  NextRow is RowNo+1, member((_,_, _,_,LineNo,RowNo),Positions), write('   '),print_line_ver(LineNo, NextRow, Positions, RowLength),!. 

print_lines(LineNo, _, LinesCount, _):- LineNo >= LinesCount.
print_lines(LineNo, Positions, LinesCount, RowLength):- NextLine is LineNo+1, print_line_hor(LineNo,0,Positions,RowLength), print_line_ver(LineNo,0,Positions,RowLength), print_lines(NextLine, Positions, LinesCount, RowLength).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_dominos :- dominos(D), frame([FrameHead|FrameTail]), dominos_positons(D, [FrameHead|FrameTail], Positions),!, place_dominos(Positions, [], Result),!, length([FrameHead|FrameTail], Rows), length(FrameHead, Columns), print_lines(0, Result, Rows, Columns).