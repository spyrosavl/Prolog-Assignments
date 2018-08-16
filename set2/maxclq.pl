:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).


create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

maxclq(N, D, Result, Size) :-
   create_graph(N,D,G),
   normalize(G, Graph),
   length(Solution, N),
   Solution #:: 0..1,
   constrain(Solution, Graph, 1),
   Cost #= -sum(Solution),
   bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
      Cost, bb_options{strategy:restart}),
   construct_result(Solution, Result,1),
   Size is -Cost.

constrain([], _, _).
constrain([H|Xs], Graph, No) :-
   Next is No+1,
   connected(H,No,Xs,Graph,Next),
   constrain(Xs, Graph, Next).

connected(_, _, [], _ , _).
connected(IsInClq, Node, [H|Nodes], Graph, No):- 
   findall(No, member(Node - No,Graph), L),
   length(L, Len),
   100*IsInClq + 10*H + Len #\= 110,
   Next is No+1,
   connected(IsInClq,Node,Nodes,Graph,Next).

normalize(List, Result) :- findall(X1 - Y1, (member(X - Y, List) , X1 is min(X,Y), Y1 is max(X,Y)), Result).

construct_result([],[],_).
construct_result([1|T],[No|Tail],No):- Next is No+1, construct_result(T,Tail,Next).
construct_result([0|T],Tail,No):- Next is No+1, construct_result(T,Tail,Next).