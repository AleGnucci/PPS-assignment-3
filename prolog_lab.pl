% search(Elem,List)
search(X,[X|_]).
search(X,[_|Xs]):-search(X,Xs).

% search2(Elem,List)
% looks for two consecutive occurrences of Elem
search_two(X,[X,_,X|_]).
search_two(X,[_|T]):-search_two(X,T).

% search_anytwo(Elem,List)
% looks for any Elem that occurs two times
search_anytwo(X,[X|T]) :- search(X, T).
search_anytwo(X,[_|T]):- search_anytwo(X,T).

% size(List,Size)
% Size will contain the number of elements in List
% it is not fully relational (loops forever when calculating list from size)
size([],0).
size([_|T],M) :- size(T,N), M is N+1.

% size(List,Size)
% Size will contain the number of elements in List,
% written using notation zero, s(zero), s(s(zero))..
% it is fully relational
sizeP([],zero).
sizeP([_|T],s(N)) :- sizeP(T,N).

% sum(List,Sum)
sum([], 0).
sum([X], X).
sum([H1, H2 | T], X):- sum(T, X2), X is H1+H2+X2.

% max(List,Max)
% Max is the biggest element in List
% Suppose the list has at least one element
max(L,A) :- max(L,0,A).
%max(List,TempMax,Max)
max([], S, S).
max([X|Xs],S,A) :- X>=S, max(Xs,X,A).
max([X|Xs],S,A) :- X<S, max(Xs,S,A).

% same(List1,List2)
% are the two lists the same?
% this predicate is fully relational
same([],[]).
same([X|Xs],[X|Ys]):- same(Xs,Ys).

% all_bigger(List1,List2)
% all elements in List1 are bigger than those in List2, 1 by 1
% example: all_bigger([10,20,30,40],[9,19,29,39]).
all_bigger([X], [Y]) :- bigger(X, Y).
all_bigger([X|Xs],[Y|Ys]):- bigger(X, Y), all_bigger(Xs,Ys).
bigger(X, Y) :- X>Y.

% sublist(List1,List2)
% List1 should be a subset of List2
% example: sublist([1,2],[5,3,2,1]).
sublist([], _).
sublist([X|Xs],Y):- search(X, Y), sublist(Xs,Y).

% seq(N,List)
% example: seq(5,[0,0,0,0,0]).
 %it is not fully relational (can't find N from List)
seq(0,[]).
seq(N,[0|T]):- N > 0, N2 is N-1, seq(N2,T).

% seqR(N,List)
% example: seqR(4,[4,3,2,1,0]).
seqR(0, [0]).
seqR(N, [N|T]) :- M is N-1, seqR(M, T).

% seqR2(N,List)
% example: seqR2(4,[0,1,2,3,4]).
seqR2(N, L) :- seqR2(N, [], L).
seqR2(0, L, [0|L]) :- !.
seqR2(N, R, L) :- N1 is N-1, seqR2(N1, [N|R], L).

% inv(List,List)
% example: inv([1,2,3],[3,2,1]).
inv([X], [X]).
inv([H|T], LI) :- inv(T, LI2), append(LI2, [H], LI).

% double(List,List)
% suggestion: remember predicate append/3
% example: double([1,2,3],[1,2,3,1,2,3]).
double(L1, L2) :- append(L1, L1, L2).

% times(List,N,List)
% example: times([1,2,3],3,[1,2,3,1,2,3,1,2,3]).
times(L1, 1, L1) :- !.
times(L1, N, L2) :- M is N-1, times(L1, M, L3), append(L1, L3, L2).

% proj(List,List)
% example: proj([[1,2],[3,4],[5,6]],[1,3,5]).
proj([[H|T]], [H]).
proj([[H|T1]|T2], [H|T3]) :- proj(T2, T3).