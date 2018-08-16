bigger_or_equal_than([], _, 0).
bigger_or_equal_than([H|T], Y, N):- H >= Y, bigger_or_equal_than(T, Y, N1), N is N1 + 1.
bigger_or_equal_than([H|T], Y, N):- H < Y, bigger_or_equal_than(T, Y, N).

liars_helper(_, [], []).
liars_helper(List, [H|T], [0|Tail]):- bigger_or_equal_than(List, H, N), H=<N, liars_helper(List, T, Tail).
liars_helper(List, [H|T], [1|Tail]):- bigger_or_equal_than(List, H, N), H>N, liars_helper(List, T, Tail).
liars(L,R):- liars_helper(L,L,R).