:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).


tents_min(RowTents , ColumnTents , Trees , Solution, Cost) :-
   length(RowTents, N), length(ColumnTents, M), Size is N*M,
   length(Solution, Size),
   Solution #:: 0..1,
   constrain(Solution, Solution, M, RowTents , ColumnTents , Trees, 1),
   Cost #= sum(Solution),
   bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
      Cost, bb_options{strategy:restart}).

tents(RowTents , ColumnTents , Trees , Result) :-
   tents_min(RowTents , ColumnTents , Trees , _, Cost),
   length(RowTents, N), length(ColumnTents, M), Size is N*M,
   length(Solution, Size),
   Solution #:: 0..1,
   constrain(Solution, Solution, M, RowTents , ColumnTents , Trees, 1),
   Cost #= sum(Solution),
   search(Solution, 0, first_fail, indomain_middle, complete, []),
   construct_solution(Solution, 1, M, Result).


constrain([], _, _, _, _, _, _).
constrain([H|T], Solution, M, RowTents , ColumnTents, Trees, Position):-
   X is ((Position-1) div M) + 1,
   Y is ((Position-1) mod M) + 1,
   check_tree(Solution, M, X, Y, Trees, TreeNeighbours),
   length(TreeNeighbours, TreeNeighbours_Len),
   findall(1, member(X-Y, Trees), L2),
   length(L2, C2),
   list_of_neighbours(Solution, M, 1, Position, L3),
   get_n_row(Solution,X,M,1,SameRow),
   get_n_column(Solution,Y,M,1,SameColumn),
   get_nth(RowTents,X,1,RowLimit),
   get_nth(ColumnTents,Y,1,ColLimit),
   (TreeNeighbours_Len#=0) or ( (sum(TreeNeighbours)#>0) ), %periorismos gia ta dentra
   (C2#=0) or (H#=0), %periorismos gia to an yparxei dentro
   (sum(L3)#=0) or (H#=0), %periorismos gia toys geitones tis tentas
   ((RowLimit #< 0) or (sum(SameRow) #=< RowLimit)) and ((ColLimit #< 0) or (sum(SameColumn) #=< ColLimit)), %periorismos gia grammes kai styles
   NewPos is Position + 1,
   constrain(T, Solution, M, RowTents , ColumnTents , Trees, NewPos).


check_tree(Solution, M, X, Y, Trees, TreeNeighbours):- 
   is_there_tree_near(X, Y, Trees, TreeX-TreeY),
   TreePosition is (TreeX-1)*M + TreeY,
   list_of_neighbours(Solution, M, 1, TreePosition, TreeNeighbours),!.

check_tree(_, _, _, _, _, []).

is_there_tree_near(_, _, [], 0).
is_there_tree_near(X, Y, [X1-Y1|_], X1-Y1):- Dist is sqrt( (X-X1)^2 + (Y-Y1)^2), Dist < 2, Dist > 0, !.
is_there_tree_near(X, Y, [_|Tail], Result):- is_there_tree_near(X,Y, Tail, Result), !.


list_of_neighbours([],_,  _, _, []).

list_of_neighbours([H|T], Width, CurPostion, TentPosition, [H|Results]):- 
   RowDif is abs(( (CurPostion-1) div Width) - ( (TentPosition-1) div Width)),
   RowDif #= 0,
   Dist #= abs(CurPostion-TentPosition),
   Dist #= 1,
   NewPos is CurPostion+1,
   list_of_neighbours(T, Width, NewPos, TentPosition, Results), !.

list_of_neighbours([H|T], Width, CurPostion, TentPosition, [H|Results]):- 
   RowDif is abs(( (CurPostion-1) div Width) - ( (TentPosition-1) div Width)),
   RowDif #= 1,
   Dist is abs(CurPostion-TentPosition),
   Dist =< Width+1,
   Dist >= Width-1,
   NewPos is CurPostion+1,
   list_of_neighbours(T, Width, NewPos, TentPosition, Results), !.

list_of_neighbours([_|T], Width, CurPostion, TentPosition, Results):-
   NewPos is CurPostion+1,
   list_of_neighbours(T, Width, NewPos, TentPosition, Results), !.


get_n_row([],_,_,_,[]).

get_n_row([H|Tail],N,RowSize,Current,[H|More]):-
   Current #> RowSize * (N-1),
   Current #=< RowSize * N,
   NewPos is Current + 1,
   get_n_row(Tail,N,RowSize,NewPos,More),!.

get_n_row([_|Tail],N,RowSize,Current,More):-
   NewPos is Current + 1,
   get_n_row(Tail,N,RowSize,NewPos,More),!.

get_n_column([],_,_,_,[]).

get_n_column([H|Tail],N,RowSize,Current,[H|More]):-
   N #= Current mod RowSize,
   NewPos is Current + 1,
   get_n_column(Tail,N,RowSize,NewPos,More),!.

get_n_column([_|Tail],N,RowSize,Current,More):-
   NewPos is Current + 1,
   get_n_column(Tail,N,RowSize,NewPos,More),!.

get_nth([],_,_,_).
get_nth([H|_],N,Cur,H):- Cur is N, !.
get_nth([_|T],N,Cur,R):- NewCur is Cur + 1, get_nth(T,N,NewCur,R).


construct_solution([], _, _, []).

construct_solution([0|Tail], Current, Width, Results):-
   NewCur is Current + 1,
   construct_solution(Tail, NewCur, Width, Results).

construct_solution([1|Tail], Current, Width, [X-Y|Results]):-
   X is ((Current-1) div Width) + 1,
   Y is ((Current-1) mod Width) + 1,
   NewCur is Current + 1,
   construct_solution(Tail, NewCur, Width, Results).