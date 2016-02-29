/* <username and name of the author>

THIS WORK IS ENTIRELY MY OWN.

The program does <or does not, choose which is appropriate> produce multiple answers.

I have <or I have not, choose which is appropriate> used built-ins.

1. <Number of elements in the list binding Q after executing s1(Q,100)>
<At most 300 characters of clear text on the main idea for the definition of s1.>

2. <Number of elements in the list binding Q after executing s2(Q,100)>
<At most 300 characters of clear text on the main idea for the definition of s2.>

3. <Number of elements in the list binding Q after executing s3(Q,100)>
<At most 300 characters of clear text on the main idea for the definition of s3.>

4.
<At most 300 characters of clear text on the main idea for the definition of s4.>

5. <Number of elements in the list binding Q after executing s4(Q,500)>
s4(Q,500) uses <number> inferences. */


/* <BODY OF THE PROGRAM> */

s1(Q, N) :-
        s0(Q1, N),
        bs_prods(Q1, Q2),
        rup(Q2, Q).

% generate list of all possible quadruples
s0(Q, N) :-
        N >= 5,
        X is 2,
        Y is 3,
        S is X + Y,
        P is X * Y,
        add_to_list(N, [X,Y,S,P], Q).

add_to_list(_, [0,0,0,0], []) :- !.
add_to_list(N, [X, Y, S, P], [[X, Y, S, P]|M]):-
        next(X, Y, N, NewX, NewY),
        NewS is NewX + NewY,
        NewP is NewX * NewY,
        add_to_list(N, [NewX, NewY, NewS, NewP], M).

% add 1 to y, if sum goes over fail, if under cut
next(X, Y, N, NewX, NewY) :-
        NewY is Y + 1,
        X + NewY =< N,
        !,
        NewX is X.

% add 1 to x, y is x + 1, if sum goes overn fail
next(X, _, N, NewX, NewY) :-
        NewX is X + 1,
        NewY is NewX + 1,
        NewX + NewY =< N,
        !.
        
% if no more options set to zero
next(_, _, _, NewX, NewY) :-
        NewX = 0,
        NewY = 0.

% bubble sort products
bs_prods_pass([[X1,Y1,S1,P1],[X2,Y2,S2,P2]|Z],[[X1,Y1,S1,P1]|W], N) :- 
        P1 =< P2,
        !,
        bs_prods_pass([[X2,Y2,S2,P2]|Z],W,N).
bs_prods_pass([A,B|Z],[B|W],s(N)) :- 
        bs_prods_pass([A|Z],W,N),
        !.
bs_prods_pass([A], [A], 0) :- !.
bs_prods_pass([], [], 0).

bs_prods(In, Out) :- 
        bs_prods_pass(In, Part, s(_)),
        !,
        bs_prods(Part, Out).
bs_prods(In, In).

% bubble sort sums 
bs_sums_pass([[X1,Y1,S1,P1],[X2,Y2,S2,P2]|Z],[[X1,Y1,S1,P1]|W], N) :- 
        S1 =< S2,
        !,
        bs_sums_pass([[X2,Y2,S2,P2]|Z],W,N).
bs_sums_pass([A,B|Z],[B|W],s(N)) :- 
        bs_sums_pass([A|Z],W,N),
        !.
bs_sums_pass([A], [A], 0) :- !.
bs_sums_pass([], [], 0).

bs_sums(In, Out) :- 
        bs_sums_pass(In, Part, s(_)),
        !,
        bs_sums(Part, Out).
bs_sums(In, In).

% remove unique products
rup([[X1,Y1,S1,P],[X2,Y2,S2,P],[X3,Y3,S3,P]|T], [[X1,Y1,S1,P]|L]) :-
        !,
        rup([[X2,Y2,S2,P],[X3,Y3,S3,P]|T], L).
rup([[X1,Y1,S1,P],[X2,Y2,S2,P]|T], [[X1,Y1,S1,P],[X2,Y2,S2,P]|L]) :-
        !,
        rup(T, L).
rup([_|T], L) :-
        rup(T, L),
        !.
rup([],[]).

% remove duplicate products
rdp([[_,_,_,P],[_,_,_,P],[X3,Y3,S3,P]|T], L) :-
        !,
        rdp([[X3, Y3, S3, P]|T], L).
rdp([[_,_,_,P],[_,_,_,P]|T], L) :-
        !,
        rdp(T, L).
rdp([E|T], [E|L]) :-
        rdp(T, L),
        !.
rdp([],[]).

/*


?- consult(<username>).
% <username> compiled 0.00 sec, <...> bytes
true.
?- time(s1(Q,100)).
% <...> inferences, <...> CPU in <...> seconds (100% CPU, <...> Lips)
Q = [[3,4,7,12],[2,6,8,12], <rest of complete list of quadruples>]
?- time(s2(Q,100)).
Q=<...>
<...>
*/
