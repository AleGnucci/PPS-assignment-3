% dropFirst(?Elem,?List,?OutList)
% drops only the first occurrence (showing no alternative results)
dropFirst(X,[X|T],T) :- !.
dropFirst(X, [H|Xs], [H|L]) :- dropFirst(X, Xs, L).

% dropLast(?Elem,?List,?OutList)
% drops only the last occurrence (showing no alternative results)
dropLast(X,L, O) :- reverse(L, LR), dropFirst(X, LR, LR1), reverse(LR1, O).

% dropAll(?Elem,?List,?OutList)
% drops all occurrences, returning a single list as result
dropAll(X, [], []).
dropAll(X,[X|T],R) :- !, dropAll(X,T,R).
dropAll(X,[Y|T],[Y|R]) :- dropAll(X,T,R).

% fromCircList(+List,-Graph)
% Obtains a graph from a circular list
fromCircList([H|T], G) :- fromCircList2([H|T], G, H).
fromCircList2([F],[e(F, H)], H).
fromCircList2([E1,E2|T],[e(E1,E2)|L],H) :- fromCircList2([E2|T],L,H).

% reaching(+Graph, +Node, -List)
% all the nodes that can be reached in 1 step from Node
% possibly use findall, looking for e(Node,_) combined
% with member(?Elem,?List)
reaching([], N, []) :- !.
reaching([e(N, N1)|T], N, [N1|R]) :- reaching(T, N, R), !.
reaching([H|T], N, R) :- reaching(T, N, R).

% anypath(+Graph, +Node1, +Node2, -ListPath)
% a path from Node1 to Node2
% if there are many path, they are showed 1-by-1
anypath(G, N1, N2, [e(N1, N2)]) :- member(e(N1, N2), G).
anypath(G, N1, N2, [e(N1, N3)|R]) :- member(e(N1, N3), G), anypath(G, N3, N2, R).

% allreaching(+Graph, +Node, -List)
% all the nodes that can be reached from Node
% Suppose the graph is NOT circular!
% Use findall and anyPath!
allreaching(G, N, L) :- findall(N1, anypath(G, N, N1, L1), L).
%alternative:
%the ^ operator is used so that setof excludes L1 from the free variables
allreaching2(G, N, L) :- setof(N1, L1^anypath(G, N, N1, L1), L), !. %setof eliminates duplicates


%TicTacToe
%next(@Table,@Player,-Result,-NewTable)
%example: next([cell(1, 1, x)], o, R, N).
next(T, P, R, N) :- between(0, 2, Row), 
	between(0, 2, Col), 
	not member(cell(Row, Col, _), T), 
	append(T, [cell(Row, Col, P)], N),
	result(N, R).
	
%result(+Table, -Result)
result(T, win(P)) :- rowWin(T, P), !.
result(T, win(P)) :- colWin(T, P), !.
result(T, win(P)) :- diagWin(T, P), !.
result(T, even) :- length(T, 9), !.
result(T, nothing).

%between(+Low, +High, -Value)
between(N, M, N) :- N =< M.
between(N, M, K) :- N < M, N1 is N+1, between(N1, M, K).

%rowWin(+Table, +Player)
rowWin(T, P) :- between(0, 2, Row), winInRow(T, P, Row).
winInRow(T, P, Row) :- between(0, 2, Col), winInRowOrCol(T, P, Row, Col).
winInRowOrCol(T, P, Row, Col) :- findall(count, member(cell(Row, Col, P), T), S),
	length(S, 3),
	member(cell(Row, Col, P), T). %this is needed to set P as x or o
	
%colWin(+Table, +Player)
colWin(T, P) :- between(0, 2, Col), winInCol(T, P, Col).
winInCol(T, P, Col) :- between(0, 2, Row), winInRowOrCol(T, P, Row, Col).

%diagWin(+Table, +Player)
diagWin(T, P) :- member(cell(1, 1, P), T), (winInDiag1(T, P) ; winInDiag2(T, P)).
winInDiag1(T, P) :- findall(count, member(cell(Num, Num, P), T), S), length(S, 3).
winInDiag2(T, P) :- findall(count, (between(0, 2, Row), Col is 2-Row, member(cell(Row, Col, P), T)), S), 
	length(S, 3).

%game(@Table,@Player,-Result,-TableList)
%example: game([], x, R, TL).
game(T, _, win(P), []) :- result(T, win(P)), !.
game(T, _, even, []) :- result(T, even), !.
game(Tab, P, R, [Tab|T]) :- next(Tab, P, R1, N), otherP(P, P1), game(N, P1, R, T).
otherP(x, o).
otherP(o, x). 