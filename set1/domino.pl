dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),
                (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),
                      (2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),
                            (3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),
                                  (4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),
                                        (5,5),(5,6),(5,7),(5,8),(5,9),(5,a),
                                              (6,6),(6,7),(6,8),(6,9),(6,a),
                                                    (7,7),(7,8),(7,9),(7,a),
                                                          (8,8),(8,9),(8,a),
                                                                (9,9),(9,a),
                                                                	(a,a)]).

frame([[6,5,0,5,5,3,3,1,1,4,6],
        [1,2,2,a,a,5,7,0,1,0,7],
        [5,8,6,0,8,0,9,7,7,4,2],
        [4,0,9,0,7,7,9,9,8,8,0],
        [1,a,3,8,8,5,a,8,0,0,3],
        [9,2,3,5,7,6,9,1,6,3,9],
        [2,2,2,5,8,6,0,4,6,a,a],
        [9,4,2,1,7,9,5,4,a,4,a],
        [9,a,4,9,5,5,6,6,0,a,2]]).

print_line_hor([], []).
print_line_hor([H|Line], []):- write(H), write(' '), print_line_hor(Line, []).
print_line_hor([H1,H2|Line],[h|Solution]):- write(H1), write('-'), write(H2), write(' '), print_line_hor(Line, Solution).
print_line_hor([H|Line],[v|Solution]):- write(H), write(' '), print_line_hor(Line, Solution).
print_line_hor([H|Line],[-|Solution]):- write(H), write(' '), print_line_hor(Line, Solution).

print_line_ver([], []).
print_line_ver([_|Line], []):- write('   '), print_line_ver(Line, []).
print_line_ver([_,_|Line],[h|Solution]):- write('       '), print_line_ver(Line, Solution).
print_line_ver([_|Line],[v|Solution]):- write('|'), write('  '), print_line_ver(Line, Solution). 
print_line_ver([_|Line],[-|Solution]):- write('   '), print_line_ver(Line, Solution).

print_line(Line,Solution):- print_line_hor(Line, Solution), nl, print_line_ver(Line, Solution), nl.

print_grid([], []).
print_grid([H|Lines], [H1|Solutions]):- print_line(H,H1), print_grid(Lines,Solutions).

my_select([], _, _):- fail.
my_select(_, (_,[]) , _):- fail.
my_select(_, ([],_) , _):- fail.
my_select(L1, (X,Y), L2):- member((X,Y), L1), subtract(L1, [(X,Y)], L2).
my_select(L1, (X,Y), L2):- member((Y,X), L1), subtract(L1, [(Y,X)], L2).

solve_line([[_]],Dominos, [], [], Dominos).
solve_line([[]], Dominos, [], [], Dominos).
solve_line([[],[]], Dominos, [], [], Dominos).


%exei idi syndethei katheta
solve_line([[x|T1]], Dominos, [-|Tail], [_|EditedLine], Remaineddominos):- solve_line([T1], Dominos, Tail, EditedLine, Remaineddominos).
solve_line([[x|T1],[H1|T2]], Dominos, [-|Tail], [H1|EditedLine], Remaineddominos):- solve_line([T1,T2], Dominos, Tail, EditedLine, Remaineddominos).

%orizonties syndeseis
solve_line([[H1,H2|T1],[H3,H4|T2]], Dominos, [h|Tail], [H3,H4|EditedLine], Remaineddominos):- my_select(Dominos, (H1,H2), Restdominos), solve_line([T1,T2], Restdominos, Tail, EditedLine, Remaineddominos).
%orizonties an eimaste stin teleftea grammi
solve_line([[H1,H2|T1]], Dominos, [h|Tail], [], Remaineddominos):- my_select(Dominos, (H1,H2), Restdominos), solve_line([T1], Restdominos, Tail, _, Remaineddominos).

%kathetes me ena stoixeio
solve_line([[H1],[H2]], Dominos, [v], [x], Remaineddominos):- my_select(Dominos, (H1,H2), Remaineddominos).

%kathetes me pano apo 1 stoixeia
solve_line([[H1|T1],[H2|T2]], Dominos, [v|Tail], [x|EditedLine], Remaineddominos):- my_select(Dominos, (H1,H2), Restdominos), solve_line([T1,T2], Restdominos, Tail, EditedLine, Remaineddominos).

solve([], _, []).
solve([H1], Dominos, [Result]):- solve_line([H1], Dominos, Result, _, _).
solve([H1,H2|T], Dominos, Result):- solve_line([H1,H2], Dominos, R1, H2Edited, RestDominos), solve([H2Edited|T], RestDominos, R2), append([R1], R2, Result). 

put_dominos:- dominos(D), frame(F), solve( F, D, Solution), print_grid( F, Solution).