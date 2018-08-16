:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).

liars_csp(Input, Solution) :-
   length(Input, N),
   length(Solution, N),
   Solution #:: 0..1,
   constrain(Solution, Input, Input),
   search(Solution, 0, first_fail, indomain_middle, complete, []).

constrain([], [], _).
constrain([H|Xs], [In|Tail], Input) :-
   findall(X, (member(X,Input), X #>= In), R),
   length(R, Len),
   H #= In #> Len,
   constrain(Xs, Tail, Input).

genrand(N, List) :-
   length(List, N),
   make_list(N, List).
   make_list(_, []).
make_list(N, [X|List]) :-
   random(R),
   X is R mod (N+1),
   make_list(N, List).