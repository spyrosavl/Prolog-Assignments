:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).

take([], _, []).
take(_, 0, []).
take([H|T], N, [H|Tail]):- N > 0, New is N - 1, take(T, New, Tail).

hcvrp(NCl, NVe, Timeout, Result, Cost, Time):-
	vehicles(V0), clients(C0), take(V0,NVe,Ve), take(C0,NCl,Cl),
	Size is NCl * NVe,
	length(Solution, Size),
	Solution #:: 0..NCl,
	order_contraints(Solution, NCl, NVe, 1),
	occurences_constrain(NCl, Solution),
	weight_constrain(Solution, 1, NVe, Ve, Cl),
	clients_with_warehouse(Cl, ClientsWithWareHouse),
	distance_list(ClientsWithWareHouse, ClientsWithWareHouse, DistanceList),
	distance_cost(Solution, DistanceList, NVe, NCl, Distances),
	Cost #= sum(Distances),
	cputime(T1),
	bb_min(search(Solution, 0, first_fail, indomain_middle, complete, []),
	  Cost, bb_options{strategy:restart, timeout:Timeout}),
	cputime(T2),
	construct_solution(Solution, 1, NCl, _,Result),
	Time is T2 - T1.

clients_with_warehouse(Clients, Result):- append([c(0,0,0)],Clients,Result).

distance_cost(_, _, 0, _, []).
distance_cost(Solution, DistanceList, TruckNo, NoOfClients, [Dist|Distances]):-
	truck_vars(Solution, NoOfClients, TruckNo, 1, TruckVars),
	truck_cost(TruckVars, DistanceList, 0, 0, NoOfClients, Dist),
	Next is TruckNo - 1,
	distance_cost(Solution, DistanceList, Next, NoOfClients, Distances).

truck_cost([], DistanceList, LastNotZero, _, _, Distance):-
	DistPosition #= 1 + LastNotZero,
	element(DistPosition, DistanceList, Distance).
truck_cost([Var|Vars],DistanceList, LastNotZero, FromClient, NoOfClients, Distance):-
	(Var #> 0) => (DistPosition #= ((NoOfClients+1) * FromClient + 1) + Var),
	(Var #= 0) => (DistPosition #= 1),
	element(DistPosition, DistanceList, TmpDist1),
	(Var #> 0) => (NewLastNotZero #= Var),
	(Var #= 0) => (NewLastNotZero #= LastNotZero),
	truck_cost(Vars,DistanceList, NewLastNotZero, Var, NoOfClients, TmpDist2),
	Distance #= TmpDist1 + TmpDist2.

distance_list([], _, []).
distance_list([c(_,CX,CY)|Tail], Clients, Result):-
	distance_list_from_one_client(CX,CY, Clients, R1),
	distance_list(Tail, Clients, R2),
	append(R1,R2,Result).

distance_list_from_one_client(_,_,[],[]).
distance_list_from_one_client(X,Y,[c(_,CX,CY)|Clients],[D|Tail]):-
	distance(X,Y,CX,CY,D),
	distance_list_from_one_client(X,Y,Clients,Tail).

distance(X1,Y1,X2,Y2,Cost):- Cost1 is round(1000*sqrt((X1-X2)^2+(Y1-Y2)^2)), fix(Cost1, Cost).

occurences_constrain(0, _).
occurences_constrain(NCl, Solution):- occurrences(NCl, Solution, 1), NewNCl is NCl-1, occurences_constrain(NewNCl, Solution).

truck_vars([], _, _, _, []).
truck_vars([Var|Vars], VarsPerTruck, TruckNo, Current, [Var|Tail]):-
	Current > (TruckNo-1)*VarsPerTruck,
	Current =< TruckNo*VarsPerTruck,
	New is Current+1,
	truck_vars(Vars, VarsPerTruck, TruckNo, New, Tail),!.
truck_vars([_|Vars], VarsPerTruck, TruckNo, Current, Tail):-
	New is Current+1,
	truck_vars(Vars, VarsPerTruck, TruckNo, New, Tail),!.

clients_weights([], []).
clients_weights([c(W,_,_)|Tail], [W|Weights]):- clients_weights(Tail, Weights).

weight_constrain(_, TruckNo, NoOfVehicles, _, _):- TruckNo > NoOfVehicles.
weight_constrain(Solution, TruckNo, NoOfVehicles, [Vehicle|Vehicles], Cl):-
	length(Cl, NoOfClients),
	TruckNo =< NoOfVehicles,
	truck_vars(Solution, NoOfClients, TruckNo, 1, TruckVars),
	clients_weights(Cl, ClientsWeights),
	weight_constrain_truck(TruckVars, ClientsWeights, TruckWeights),
	Vehicle #>= sum(TruckWeights),
	Next is TruckNo + 1,
	weight_constrain(Solution, Next, NoOfVehicles, Vehicles, Cl).

weight_constrain_truck([], _, []).
weight_constrain_truck([Var|Vars], ClientsWeights, [W|Weights]):-
	(Var #> 0) => (Position #= Var),
	(Var #= 0) => (Position #= 1),
	element(Position, ClientsWeights, W1),
	(Var #> 0) => (W #= W1),
	(Var #= 0) => (W #= 0),
	weight_constrain_truck(Vars, ClientsWeights, Weights).

order_contraints(_, _, 0, _).
order_contraints(Solution, NoOfClients, TruckNo, PrevSum):-
	truck_vars(Solution, NoOfClients, TruckNo, 1, TruckVars),
	zeros_in_the_end(TruckVars, 1),
	Sum #= sum(TruckVars),
	(PrevSum #= 0) => ( Sum #= 0 ),
	Next is TruckNo - 1,
	order_contraints(Solution, NoOfClients, Next, Sum).

zeros_in_the_end([],_).
zeros_in_the_end([H|T], Prev):-
	(Prev #= 0) => (H #= 0),
	zeros_in_the_end(T, H).

construct_solution([], _, _, [], []).

construct_solution([H|Tail], Current, ClientsNo, [], [[H|TmpResults]|Results]):-
	H > 0,
	Current is 1,
	construct_solution(Tail, 2, ClientsNo, TmpResults, Results).

construct_solution([H|Tail], Current, ClientsNo, [H|TmpResults], Results):-
	H > 0,
	Current =< ClientsNo, Current > 1,
	Next is Current+1,
	construct_solution(Tail, Next, ClientsNo, TmpResults, Results).

construct_solution([H|Tail], Current, ClientsNo, [], [TmpResults|Results]):-
	H is 0,
	Current is 1,
	construct_solution(Tail, 2, ClientsNo, TmpResults, Results).

construct_solution([H|Tail], Current, ClientsNo, TmpResults, Results):-
	H is 0,
	Current =< ClientsNo, Current > 1,
	Next is Current+1,
	construct_solution(Tail, Next, ClientsNo, TmpResults, Results).

construct_solution(Solution, Current, ClientsNo, [], Results):-
	Current > ClientsNo,
	construct_solution(Solution, 1, ClientsNo, [], Results).