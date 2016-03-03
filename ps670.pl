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
        gus(Q2, Q).

s3(Q, N) :-
        s2(Q1, N),
        merge_sort_p(Q1, Q2),
        gup(Q2, Q).

s2(Q, N) :-
        s0(ALL, N),
        merge_sort_p(ALL, ALL_SORTED_P),

        gup(ALL_SORTED_P, UNIQUE_P),
        merge_sort_s(UNIQUE_P, UNIQUE_P_SORTED_S),

        gdp(ALL_SORTED_P, DUPLICATE_P),
        merge_sort_s(DUPLICATE_P, DUPLICATE_P_SORTED_S),

        rms(UNIQUE_P_SORTED_S, DUPLICATE_P_SORTED_S, Q).

s1(Q, N) :-
        s0(Q1, N),
        merge_sort_p(Q1, Q2),
        gdp(Q2, Q).

% generate list of all possible quadgdples
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

% merge sort products
merge_sort_p([],[]) :- !.
merge_sort_p([X],[X]) :- !.
merge_sort_p(List,Sorted):-
        !,
        % list must have atlest two elements
        List = [_,_|_],
        % divide list into two parts
        divide(List,List1,List2),

        % merge sort both parts
        merge_sort_p(List1,Sorted1),
        merge_sort_p(List2,Sorted2),

        % merge both
        merge_p(Sorted1,Sorted2,Sorted).

merge_p([],L,L) :- !.
merge_p(L,[],L) :- !.
merge_p([[X1,Y1,S1,P1]|T1],[[X2,Y2,S2,P2]|T2],[[X1,Y1,S1,P1]|T]):- 
        P1 =< P2,
        !,
        merge_p(T1,[[X2,Y2,S2,P2]|T2],T).
merge_p([[X1,Y1,S1,P1]|T1],[[X2,Y2,S2,P2]|T2],[[X2,Y2,S2,P2]|T]):- 
        P1 > P2,
        !,
        merge_p([[X1,Y1,S1,P1]|T1],T2,T).

% merge sort sums
merge_sort_s([],[]) :- !.
merge_sort_s([X],[X]) :- !.
merge_sort_s(List,Sorted):-
        !,
        List = [_,_|_],
        divide(List,List1,List2),
        merge_sort_s(List1,Sorted1),
        merge_sort_s(List2,Sorted2),
        merge_s(Sorted1,Sorted2,Sorted).

merge_s([],L,L) :- !.
merge_s(L,[],L) :- !.
merge_s([[X1,Y1,S1,P1]|T1],[[X2,Y2,S2,P2]|T2],[[X1,Y1,S1,P1]|T]):- 
        S1 =< S2,
        !,
        merge_s(T1,[[X2,Y2,S2,P2]|T2],T).
merge_s([[X1,Y1,S1,P1]|T1],[[X2,Y2,S2,P2]|T2],[[X2,Y2,S2,P2]|T]):- 
        S1 > S2,
        !,
        merge_s([[X1,Y1,S1,P1]|T1],T2,T).

divide(L,List1,List2):-
        split(L,List1,List2).
   
split([X,Y|T], [X|List1], [Y|List2]) :- 
        !,
        split(T, List1, List2).
split([X], [X|List1], List2) :-
        !,
        split([], List1, List2).
split([], [], []).

% remove unique products
% get duplicate products
gdp([[X1,Y1,S1,P],[X2,Y2,S2,P],[X3,Y3,S3,P]|T], [[X1,Y1,S1,P]|L]) :-
        !,
        gdp([[X2,Y2,S2,P],[X3,Y3,S3,P]|T], L).
gdp([[X1,Y1,S1,P],[X2,Y2,S2,P]|T], [[X1,Y1,S1,P],[X2,Y2,S2,P]|L]) :-
        !,
        gdp(T, L).
gdp([_|T], L) :-
        !,
        gdp(T, L).
gdp([],[]).

% remove duplicate products
% returns unique products
gup([[_,_,_,P],[X2,Y2,S2,P],[X3,Y3,S3,P]|T], L) :-
        !,
        gup([[X2,Y2,S2,P],[X3,Y3,S3,P]|T], L).
gup([[_,_,_,P],[_,_,_,P]|T], L) :-
        !,
        gup(T, L).
gup([E|T], [E|L]) :-
        !,
        gup(T, L).
gup([],[]).

% remove duplicate sums 
% returns unique sums 
gus([[_,_,S,_],[X2,Y2,S,P2],[X3,Y3,S,P3]|T], L) :-
        !,
        gus([[X2,Y2,S,P2],[X3,Y3,S,P3]|T], L).
gus([[_,_,S,_],[_,_,S,_]|T], L) :-
        !,
        gus(T, L).
gus([E|T], [E|L]) :-
        !,
        gus(T, L).
gus([],[]).

% remove first list from second and return remainder
rms([], M, M) :- !.
rms(_, [], []) :- !.
rms([[X,Y,S,P]|N], [[_,_,S,_]|M], L) :-
        !,
        rms([[X,Y,S,P]|N], M, L).
rms([[_,_,S1,_]|N], [[X2,Y2,S2,P2]|M], L) :-
        S1<S2,
        !,
        rms(N, [[X2,Y2,S2,P2]|M], L).
rms(N, [E|M], [E|L]):-
        rms( N, M, L).

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
