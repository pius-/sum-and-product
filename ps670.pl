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

s0(Q, N) :-
        genList(N, Q1),
        bs(Q1, Q).

% generate list of all possible quadruples
genList(N, L) :-
        N >= 5,
        X is 2,
        Y is 3,
        S is X + Y,
        P is X * Y,
        makelist(N, [X,Y,S,P], L).

makelist(_, [0,0,0,0], []) :- !.
makelist(N, [X, Y, S, P], [[X, Y, S, P]|M]):-
        next(X, Y, N, NewX, NewY),
        NewS is NewX + NewY,
        NewP is NewX * NewY,
        makelist(N, [NewX, NewY, NewS, NewP], M).

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
bs_pass([[X1,X2,X3,X4],[Y1,Y2,Y3,Y4]|Z],[[X1,X2,X3,X4]|W], N) :- 
        X4 =< Y4,
        !,
        bs_pass([[Y1,Y2,Y3,Y4]|Z],W,N).
bs_pass([X,Y|Z],[Y|W],s(N)) :- 
        bs_pass([X|Z],W,N),
        !.
bs_pass([X], [X], 0) :- !.
bs_pass([], [], 0).

bs(In, Out) :- 
        bs_pass(In, Part, s(_)),
        !,
        bs(Part, Out).
bs(In, In).




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
