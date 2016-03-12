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

s4(Q, N) :-
        s3(Q1, N),
        merge_sort_s(Q1, Q2),
        guds(Q2, Q, _).

s3(Q, N) :-
        s2(Q1, N),
        merge_sort_p(Q1, Q2),
        gudp(Q2, Q, _).

s2(Q, N) :-
        s0(ALL, N),
        merge_sort_p(ALL, ALL_SORTED_P),
        gudp(ALL_SORTED_P, UNIQUE_P, DUPLICATE_P),
        merge_sort_s(UNIQUE_P, UNIQUE_P_SORTED_S),
        merge_sort_s(DUPLICATE_P, DUPLICATE_P_SORTED_S),
        rms(UNIQUE_P_SORTED_S, DUPLICATE_P_SORTED_S, Q).

s1(Q, N) :-
        s0(Q1, N),
        merge_sort_p(Q1, Q2),
        gudp(Q2, _, Q).

% generate list of all possible quadgdples
s0(Q, N) :-
        X is 2,
        Y is 3,
        S is X + Y,
        P is X * Y,
        add_to_list(N, [X,Y,S,P], Q).

add_to_list(N, [X, Y, S, P], [[X, Y, S, P]|M]):-
        next(X, Y, N, NEW_X, NEW_Y),
        NEW_S is NEW_X + NEW_Y,
        NEW_P is NEW_X * NEW_Y,
        add_to_list(N, [NEW_X, NEW_Y, NEW_S, NEW_P], M),
        !.
add_to_list(_, _, []).

% add 1 to y, if sum goes over fail, if under cut
next(X, Y, N, NEW_X, NEW_Y) :-
        NEW_Y is Y + 1,
        X + NEW_Y =< N,
        !,
        NEW_X is X.

% add 1 to x, y is x + 1, if sum goes overn fail
next(X, _, N, NEW_X, NEW_Y) :-
        NEW_X is X + 1,
        NEW_Y is NEW_X + 1,
        NEW_X + NEW_Y =< N,
        !.

% merge sort products
merge_sort_p([], []) :- !.
merge_sort_p([X], [X]) :- !.
merge_sort_p(L, S):-
        !,
        % list must have atlest two elements
        L = [_, _|_],
        % divide list into two parts
        divide(L, L1, L2),

        % merge sort both parts
        merge_sort_p(L1, S1),
        merge_sort_p(L2, S2),

        % merge both
        merge_p(S1, S2, S).

merge_p([],L,L) :- !.
merge_p(L,[],L) :- !.
merge_p([[X1,Y1,S1,P1]|T1], [[X2,Y2,S2,P2]|T2], [[X1,Y1,S1,P1]|T]):- 
        P1 =< P2,
        !,
        merge_p(T1, [[X2,Y2,S2,P2]|T2], T).
merge_p([[X1,Y1,S1,P1]|T1], [[X2,Y2,S2,P2]|T2], [[X2,Y2,S2,P2]|T]):- 
        P1 > P2,
        !,
        merge_p([[X1,Y1,S1,P1]|T1], T2, T).

% merge sort sums
merge_sort_s([],[]) :- !.
merge_sort_s([X],[X]) :- !.
merge_sort_s(L,S):-
        !,
        L = [_, _|_],
        divide(L, L1, L2),
        merge_sort_s(L1, S1),
        merge_sort_s(L2, S2),
        merge_s(S1, S2, S).

merge_s([], L, L) :- !.
merge_s(L, [], L) :- !.
merge_s([[X1,Y1,S1,P1]|T1], [[X2,Y2,S2,P2]|T2], [[X1,Y1,S1,P1]|T]):- 
        S1 =< S2,
        !,
        merge_s(T1, [[X2,Y2,S2,P2]|T2], T).
merge_s([[X1,Y1,S1,P1]|T1], [[X2,Y2,S2,P2]|T2], [[X2,Y2,S2,P2]|T]):- 
        S1 > S2,
        !,
        merge_s([[X1,Y1,S1,P1]|T1], T2, T).

divide(L, L1, L2):-
        split(L, L, L1, L2).
   
split([], R, [], R).
split([_], R, [], R).
split([_,_|T], [X|L], [X|L1], R):-
        split(T, L, L1, R),
        !.

% get unique and dup products
gudp([[X1,Y1,S1,P],[X2,Y2,S2,P],[X3,Y3,S3,P]|T], U, [[X1,Y1,S1,P]|D]) :-
        !,
        gudp([[X2,Y2,S2,P],[X3,Y3,S3,P]|T], U, D).
gudp([[X1,Y1,S1,P],[X2,Y2,S2,P]|T], U, [[X1,Y1,S1,P],[X2,Y2,S2,P]|D]) :-
        !,
        gudp(T, U, D).
gudp([E|T], [E|U], D) :-
        !,
        gudp(T, U, D).
gudp([],[],[]).

% get unique and dup sums 
guds([[X1,Y1,S,P1],[X2,Y2,S,P2],[X3,Y3,S,P3]|T], U, [[X1,Y1,S,P1]|D]) :-
        !,
        guds([[X2,Y2,S,P2],[X3,Y3,S,P3]|T], U, D).
guds([[X1,Y1,S,P1],[X2,Y2,S,P2]|T], U, [[X1,Y1,S,P1],[X2,Y2,S,P2]|D]) :-
        !,
        guds(T, U, D).
guds([E|T], [E|U], D) :-
        !,
        guds(T, U, D).
guds([],[],[]).

% remove first list from second and return remainder
rms([], M, M) :- !.
rms(_, [], []) :- !.
rms([[X,Y,S,P]|N], [[_,_,S,_]|M], L) :-
        !,
        rms([[X,Y,S,P]|N], M, L).
rms([[_,_,S1,_]|N], [[X2,Y2,S2,P2]|M], L) :-
        S1 < S2,
        !,
        rms(N, [[X2,Y2,S2,P2]|M], L).
rms(N, [E|M], [E|L]):-
        rms(N, M, L).

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
