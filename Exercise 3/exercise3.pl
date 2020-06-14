/* Jasmine Cronin */

/*Question 1*/
/* Solution taken from what was covered in tutorial */

my_append([],Y,Y).
my_append([H|X],Y,[H|Z]) :- my_append(X,Y,Z).

my_reverse([],[]).
my_reverse([H|T],R) :- my_reverse(T,Tmp), my_append(Tmp,[H],R).

my_flatten([],[]).
my_flatten([H|T],R) :- my_flatten(H,TmpH), my_flatten(T,TmpT), my_append(TmpH,TmpT,R).
my_flatten([H|T],[H|R]) :- H\=[], H\=[_|_], my_flatten(T,R).

my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

my_remove(X,[X|T],T).
my_remove(X,[H|T],[H|R]) :- my_remove(X,T,R).

/*Question 2*/
/* Solution taken from what was covered in tutorial */

member2(X,[X|T]) :- my_member(X,[X]), my_member(X,T).
member2(X,[_|T]) :- member2(X,T).

/*Question 3*/
/* Solution taken from what was covered in tutorial */

substring(X,Y) :- my_append(_,Tmp,Y), my_append(X,_,Tmp).

/*Question 4*/
/* Solution taken from what was covered in tutorial */

sublists([],[]).
sublists([H|T],[H|S]) :- sublists(T,S).
sublists([_|T],S) :- sublists(T,S).

/*Question 5*/
/* Solution taken from what was covered in tutorial */

my_permutation([],[]).
my_permutation([H|T],R) :- my_permutation(T,W), my_remove(H,R,W).

/*Question 9*/
/* Solution taken from what was covered in tutorial */

josephus(N,M) :- form_a_list(1,N,L), find_winner(L,M,Win), write('The winner is '), write(Win).

form_a_list(A,B,[]) :- A > B.
form_a_list(A,B,L) :- A =< B, TmpA is A+1, form_a_list(TmpA,B,TmpL), my_append([A],TmpL,L).

find_winner([Win],_,Win).
find_winner(L,M,Win) :- cyclic(L,M,[_|TmpL]), find_winner(TmpL,M,Win).

cyclic([],_,[]).
cyclic([H|T],0,[H|T]).
cyclic([H|T],N,L) :- N>0, my_append(T,[H],Tmp), N1 is N-1, cyclic(Tmp,N1,L).