decode_rl([],[]).
decode_rl([(Symbol,0)|T],Tail):- decode_rl(T,Tail).
decode_rl([(Symbol,Times)|T],[Symbol|Tail]):- N_Times is Times-1, decode_rl([(Symbol,N_Times)|T],Tail).
decode_rl([H|T],[H|Tail]):- decode_rl(T,Tail).

encode_rl([],[]).
encode_rl([H],[H]).
encode_rl([H,H|T],[(H,N1)|Tail]):- encode_rl([H|T], [(H,N2)|Tail]), N1 is N2+1.
encode_rl([H,H|T],[(H,N1)|Tail]):- encode_rl([H|T], [H|Tail]), N1 is 2.
encode_rl([H|T],[H|Tail]):- encode_rl(T, Tail).

%H proti apantisi tha einai L = [(p(3), 2), (q(3), 2), q(4)]
%Ayto ginete giati to X kai to Y einai metavlites kai epistrefei sti thesi toys poy tha mporoysan na ikanopooysan to query
%To X ginete arxika 3 epidi o protos kanonas poy teriazei kai eksetazei H prolog einai aytos sti grammi 8