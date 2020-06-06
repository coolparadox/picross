-module(picross_combinatorics).
-compile([export_all]).

% permutations([a,b,c]) -> [[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,a,b],[c,b,a]]
permutations(L) -> permutations(L, []).
permutations([], Acc) -> Acc;
permutations([H|T], []) -> permutations(T, [[H]]);
permutations([H|T], Acc) ->
    permutations(T, lists:append(lists:map(fun(Permutation) -> rotations([H|Permutation]) end, Acc))).

% rotations([a,b,c]) -> [[c,a,b],[b,c,a],[a,b,c]]
rotations(L) -> rotations(length(L), L, []).
rotations(0, _, Acc) -> Acc;
rotations(N, [H|T], Acc) -> rotations(N-1, T++[H], [[H|T]|Acc]).

% permutations_with_repetition([a,a,c]) -> [[a,a,c],[a,c,a],[c,a,a]]
permutations_with_repetition(L) -> uniq(lists:sort(permutations(L))).

% uniq([[1,2],[1,2],[1,3]]) -> [[1,2],[1,3]]
uniq(L) -> uniq(L, []).
uniq([], Acc) -> lists:reverse(Acc);
uniq([H|T], []) -> uniq(T, [H]);
uniq([H|T], [HA|TA]) ->
    case H of
        HA -> uniq(T, [HA|TA]);
        _ -> uniq(T, [H,HA|TA])
    end.
