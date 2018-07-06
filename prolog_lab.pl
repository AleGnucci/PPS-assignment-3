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
% it is not fully relational
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
max([], S, S).
max(L,A) :- max(L,0,A).
max([X|Xs],S,A) :- X>=S, max(Xs,X,A).
max([X|Xs],S,A) :- X=<S, max(Xs,S,A).

% same(List1,List2)
% are the two lists the same?
same([],[]).
same([X|Xs],[X|Ys]):- same(Xs,Ys).

% all_bigger(List1,List2)
% all elements in List1 are bigger than those in List2, 1 by 1
% example: all_bigger([10,20,30,40],[9,19,29,39]).
all_bigger([X], [Y]) :- X>=Y.
all_bigger([X|Xs],[Y|Ys]):- X>=Y, all_bigger(Xs,Ys).

% sublist(List1,List2)
% List1 should be a subset of List2
% example: sublist([1,2],[5,3,2,1]).
sublist([], L).
sublist([X|Xs],Y):- search(X, Y), sublist(Xs,Y).

% seq(N,List)
% example: seq(5,[0,0,0,0,0]).
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