% search(Elem, List)

search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).

% search2(Elem, List)
% looks for two consecutive occurrences of Elem
search2(X, cons(X, cons(X, _))). 
search2(X, cons(_, Xs)) :- search2(X, Xs).

% search_two(Elem, List)
% looks for two occurrences of Elem with any element in between!
search_two(X, cons(X, cons(E, cons(X, _)))).
search_two(X, cons(_, Xs)) :- search_two(X, Xs).

% search_anytwo(Elem, List)
% looks for any Elem that occurs two times, anywhere
search_anytwo(X, cons(X, Xs)) :- search(X, Xs).
search_anytwo(X, cons(Y, Ys)) :- search_anytwo(X, Ys).

%%% size(List ,Size)
% Size will contain the number of elements in List, written using notation zero , s(zero), s(s(zero))..
size(nil, zero).
size(cons(_, T), S) :- size(T, s(S)).

% sum_list(List, Sum)
sum(zero, N, N).
sum(s(N), M, s(O)) :- sum(N, M, O). % s(N) + M := s(N+M)

sum_list(nil, zero).
sum_list(cons(H, T), R) :- sum(H, S, R), sum_list(T, S).


% count(List, Element, NOccurrences)
% it uses count(List, Element, NOccurrencesSoFar, NOccurrences)
count(List, E, N) :- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E, N, M).


% max(List , Max)
% Max is the biggest element in List
% Suppose the list has at least one element
greater(s(_), zero).
greater(s(E1), s(E2)) :- greater(E1, E2).

max(cons(H, T), M) :- max(T, H, M).
max(nil, TM, TM).
max(cons(E, L), E, M) :- max(L, E, M).
max(cons(E, L), TM, M) :- greater(E, TM), max(L, E, M).
max(cons(E, L), TM, M) :- greater(TM, E), max(L, TM, M).

% min-max(List,Min,Max)
% Min is the smallest element in List
% Max is the biggest element in List
% Suppose the list has at least one element
min(cons(H, T), M) :- min(T, H, M).
min(nil, TM, TM).
min(cons(E, L), E, M) :- min(L, E, M).
min(cons(E, L), TM, M) :- greater(TM, E), min(L, E, M).
min(cons(E, L), TM, M) :- greater(E, TM), min(L, TM, M).

min_max(L, MI, MA) :- min(L, MI), max(L, MA).

% same(List1 ,List2)
% are the two lists exactly the same?
same(nil, nil).
same(cons(H, T1), cons(H, T2)) :- same(T1, T2).

% all_bigger(List1,List2)
% all elements in List1 are bigger than those in List2, 1 by 1
all_bigger(nil, nil).
all_bigger(cons(H1, T1), cons(H2, T2)) :- greater(H1, H2), all_bigger(T1, T2).

% sublist(List1 ,List2)
% List1 should contain elements all also in List2
sublist(nil, L).
sublist(cons(H, T), L2) :- search(H, L2), sublist(T, L2).

% seq(N,E,List) --> List is [E,E,...,E] with size N
% example: seq(s(s(s(zero))), a, cons(a,cons(a,cons(a, nil)))).
seq(zero, _, nil).
seq(s(N), E, cons(E,T)) :- seq(N, E, T).

% seqR(N,List)
seqR(zero, nil).
seqR(s(N), cons(N, T)) :- seqR(N, T).

% seqR2(N,List) --> is [0,1,...,N-1]
% range(N, T, L)
range(E, E, nil).
range(S, E, cons(S, T)) :- range(s(S), E, T).
seq2(N, L) :- range(zero, N, L).



