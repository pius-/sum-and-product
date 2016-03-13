/* ps670, Pius Surendralal

THIS WORK IS ENTIRELY MY OWN.

The program does not produce multiple answers.

I have not used built-ins.

1. 1747 
    (a) P: I do not know the two numbers.
    
    Generate list of all quads [X, Y, S, P] where 1 < X < Y and X + Y =< N
    Sort this list in ascending order by P
    Remove the unique products

2. 145 
    (b) S: I knew you didn’t know. I don’t know either.
    
    Generate list of all quads [X, Y, S, P] where 1 < X < Y and X + Y =< N
    Sort this list in ascending order by P
    Get unique and duplicate P from this list
    Sort both unique and dup P lists by sum
    Remove S that appear in unique from duplicate

3. 86 
    (c) P: Now I know the two numbers.

    Sort result from s2 by P
    Remove duplicate P

4. 
    (d) S: Now I know the two numbers.

    Sort result from s3 by S
    Remove duplicate S

5. 1 
s4(Q,500) uses 5226320 inferences. */


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
        add_to_list(N, [2,3,5,6], Q).

% adds the given quad to the list
% and works out the next quad
add_to_list(N, [X, Y, S, P], [[X, Y, S, P]|M]):-
        next(X, Y, N, NEXT_QUAD),
        add_to_list(N, NEXT_QUAD, M),
        !.
add_to_list(_, X, [X]).

% add 1 to y, if sum goes over fail, if under cut
next(X, Y, N, [X,NEW_Y,NEW_S,NEW_P]) :-
        NEW_Y is Y + 1,
        X + NEW_Y =< N,
        !,
        NEW_S is X + NEW_Y,
        NEW_P is X * NEW_Y.

% add 1 to x, y is x + 1, if sum goes over n fail
next(X, _, N, [NEW_X,NEW_Y,NEW_S,NEW_P]) :-
        NEW_X is X + 1,
        NEW_Y is NEW_X + 1,
        NEW_X + NEW_Y =< N,
        NEW_S is NEW_X + NEW_Y,
        NEW_P is NEW_X * NEW_Y.

% merge sort products
merge_sort_p([Q], [Q]) :- !.
merge_sort_p(L, S):-
        % list must have atlest two elements
        L = [_, _|_],
        % divide list into two parts
        split(L, L, L1, L2),

        % merge sort both parts
        merge_sort_p(L1, S1),
        merge_sort_p(L2, S2),

        % merge both
        merge_p(S1, S2, S).

% merge the lists in order
merge_p([], L, L) :- !.
merge_p(L, [], L) :- !.
merge_p([[X1,Y1,S1,P1]|T1], [[X2,Y2,S2,P2]|T2], [[X1,Y1,S1,P1]|T]):- 
        P1 =< P2,
        !,
        merge_p(T1, [[X2,Y2,S2,P2]|T2], T).
merge_p([Q1|T1], [Q2|T2], [Q2|T]):- 
        merge_p([Q1|T1], T2, T).

% merge sort sums
merge_sort_s([], []) :- !.
merge_sort_s([Q], [Q]) :- !.
merge_sort_s(L, S):-
        L = [_, _|_],
        split(L, L, L1, L2),
        merge_sort_s(L1, S1),
        merge_sort_s(L2, S2),
        merge_s(S1, S2, S).

merge_s([], L, L) :- !.
merge_s(L, [], L) :- !.
merge_s([[X1,Y1,S1,P1]|T1], [[X2,Y2,S2,P2]|T2], [[X1,Y1,S1,P1]|T]):- 
        S1 =< S2,
        !,
        merge_s(T1, [[X2,Y2,S2,P2]|T2], T).
merge_s([Q1|T1], [Q2|T2], [Q2|T]):- 
        merge_s([Q1|T1], T2, T).

% split the list into two
split([], R, [], R) :- !.
split([_], R, [], R) :- !.
split([_,_|T], [Q|L], [Q|L1], R):-
        split(T, L, L1, R).

% get unique and dup products
gudp([],[],[]) :- !.
gudp([[X1,Y1,S1,P],[X2,Y2,S2,P],[X3,Y3,S3,P]|T], U, [[X1,Y1,S1,P]|D]) :-
        !,
        gudp([[X2,Y2,S2,P],[X3,Y3,S3,P]|T], U, D).
gudp([[X1,Y1,S1,P],[X2,Y2,S2,P]|T], U, [[X1,Y1,S1,P],[X2,Y2,S2,P]|D]) :-
        !,
        gudp(T, U, D).
gudp([Q|T], [Q|U], D) :-
        gudp(T, U, D).

% get unique and dup sums 
guds([],[],[]) :- !.
guds([[X1,Y1,S,P1],[X2,Y2,S,P2],[X3,Y3,S,P3]|T], U, [[X1,Y1,S,P1]|D]) :-
        !,
        guds([[X2,Y2,S,P2],[X3,Y3,S,P3]|T], U, D).
guds([[X1,Y1,S,P1],[X2,Y2,S,P2]|T], U, [[X1,Y1,S,P1],[X2,Y2,S,P2]|D]) :-
        !,
        guds(T, U, D).
guds([Q|T], [Q|U], D) :-
        guds(T, U, D).

% remove first list from second and return remainder
rms(_, [], []) :- !.
rms([[X,Y,S,P]|T1], [[_,_,S,_]|T2], L) :-
        !,
        rms([[X,Y,S,P]|T1], T2, L).
rms([[_,_,S1,_]|T1], [[X2,Y2,S2,P2]|T2], L) :-
        S1 < S2,
        !,
        rms(T1, [[X2,Y2,S2,P2]|T2], L).
rms(T1, [Q|T2], [Q|L]):-
        rms(T1, T2, L).


/*

?- consult(ps670).
true.
?- time(s1(Q,100)).
% 74,962 inferences, 0.018 CPU in 0.018 seconds (96% CPU, 4270423 Lips)
Q = [[2, 6, 8, 12], [3, 4, 7, 12], [2, 9, 11, 18], [3, 6, 9, 18], [2, 10, 12, 20], [4, 5, 9|...], [2, 12|...], [3|...], [...|...]|...].

?- time(s2(Q,100)).
% 141,819 inferences, 0.036 CPU in 0.049 seconds (74% CPU, 3915921 Lips)
Q = [[2, 9, 11, 18], [3, 8, 11, 24], [4, 7, 11, 28], [5, 6, 11, 30], [2, 15, 17, 30], [3, 14, 17|...], [4, 13|...], [5|...], [...|...]|...].

?- time(s3(Q,100)).
% 144,274 inferences, 0.035 CPU in 0.048 seconds (74% CPU, 4077149 Lips)
Q = [[2, 9, 11, 18], [3, 8, 11, 24], [4, 7, 11, 28], [2, 25, 27, 50], [4, 13, 17, 52], [2, 27, 29|...], [4, 19|...], [4|...], [...|...]|...].

?- time(s4(Q,100)).
% 145,725 inferences, 0.036 CPU in 0.048 seconds (74% CPU, 4057836 Lips)
Q = [[4, 13, 17, 52]].

?- time(s4(Q,500)).
% 5,226,320 inferences, 1.593 CPU in 1.643 seconds (97% CPU, 3361144 Lips)
Q = [[4, 13, 17, 52]].

*/
