-module(picross).
-compile(export_all).

% Calculate all combinations of a line or column of a picross puzzle.
% Parameters:
% - Marks: list of sizes of filled regions
% - Length: length of the line or column
% Each element of the answer if a list where the first element is the size of the first gap,
% followed by the size of the first region, next gap, next region and so on.
% Call sample: combinations([2,3], 10)
combinations([], _) -> [];
combinations(Marks, Length) ->
    [ [FirstGap|mix(Marks, Gaps)] || [FirstGap|Gaps] <- xfill(Length - lists:sum(Marks), length(Marks) + 1) ].

hfill(Sum, 1) -> [[Sum]];
hfill(Sum, Count) when Count > 1 andalso Sum >= Count ->
    [ [H|T] || H <- lists:seq(1,Sum-(Count-1)), T <- hfill(Sum-H, Count-1) ].

xfill(Sum, 2) ->
    [ [L, Sum - L] || L <- lists:seq(0, Sum) ];
xfill(Sum, Count) when Count > 2 andalso Sum >= Count - 2 ->
    [ [L|Middle] ++ [R] || L <- lists:seq(0,Sum-(Count-2)), R <- lists:seq(0, Sum-(Count-2)-L), Middle <- hfill(Sum-L-R, Count-2) ].

mix([], _) -> [];
mix([H1|T1], [H2|T2]) -> [H1|[H2|mix(T1,T2)]].
