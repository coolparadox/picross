-module(picross).
-compile(export_all).

% Maps = lists:map(fun picross:picr2map/1, picross:picrCombine([8], 10)).
% FirstClue = picross:emergeClue(Maps).
% NewClue = picross:emergeClue(OldClue, Maps).

emergeClue([Map|Maps]) -> emergeClue(lists:duplicate(length(Map), unknown), [Map|Maps]).

emergeClue(Clue, [Map|Maps]) ->
    case matchMapClue(Map, Clue) of
        false -> emergeClue(Clue, Maps);
        true -> emergeClue(Clue, Map, Maps)
    end.

emergeClue(_, Result, []) -> Result;
emergeClue(Clue, Result, [Map|Maps]) ->
    emergeClue(
      Clue,
      case matchMapClue(Map, Clue) of
          true->updateClue(Result, Map);
          _ -> Result
      end,
      Maps).

matchMapClue([], []) -> true;
matchMapClue([_|MapT], [unknown|ClueT]) -> matchMapClue(MapT, ClueT);
matchMapClue([gap|MapT], [gap|ClueT]) -> matchMapClue(MapT, ClueT);
matchMapClue([fill|MapT], [fill|ClueT]) -> matchMapClue(MapT, ClueT);
matchMapClue([_|_], [_|_]) -> false.

updateClue([], _) -> [];
updateClue([gap|RefT], [gap|MapT]) -> [gap|updateClue(RefT, MapT)];
updateClue([fill|RefT], [fill|MapT]) -> [fill|updateClue(RefT, MapT)];
updateClue([_|RefT], [_|MapT]) -> [unknown|updateClue(RefT, MapT)].

picr2map([0]) -> [];
picr2map([GapLen]) -> [gap|picr2map([GapLen-1])];
picr2map([0,0|Others]) -> picr2map(Others);
picr2map([0,FillLen|Others]) -> [fill|picr2map([0,FillLen-1|Others])];
picr2map([GapLen,FillLen|Others]) -> [gap|picr2map([GapLen-1,FillLen|Others])].

% Calculate all combinations of a line or column of a picross puzzle.
% Parameters:
% - Fills: list of sizes of filled regions
% - Length: length of the line or column
% Each element of the answer if a list where the first element is the size of the first gap,
% followed by the size of the first region, next gap, next region and so on.
% Call sample: picrCombine([2,3], 10)
picrCombine([], _) -> [];
picrCombine(Fills, Length) ->
    [ [FirstGap|mix(Fills, Gaps)] || [FirstGap|Gaps] <- xfill(Length - lists:sum(Fills), length(Fills) + 1) ].

% Discover all combinations of integer lists where:
% - The number of items is 'Count'
% - Each element is equal or greater than 1
% - The sum of all elements is 'Sum'
% Call sample: hfill(5,3)
hfill(Sum, 1) -> [[Sum]];
hfill(Sum, Count) when Count > 1 andalso Sum >= Count ->
    [ [H|T] || H <- lists:seq(1,Sum-(Count-1)), T <- hfill(Sum-H, Count-1) ].

% Discover all combinations of integer lists where:
% - The number of items is 'Count'
% - First and last elements are equal or greater than 0
% - Other elements are equal or greater than 1
% - The sum of all elements is 'Sum'
% Call sample: xfill(5,3)
xfill(Sum, 2) ->
    [ [L, Sum - L] || L <- lists:seq(0, Sum) ];
xfill(Sum, Count) when Count > 2 andalso Sum >= Count - 2 ->
    [ [L|Middle] ++ [R] || L <- lists:seq(0,Sum-(Count-2)), R <- lists:seq(0, Sum-(Count-2)-L), Middle <- hfill(Sum-L-R, Count-2) ].

% Mix two lists, alternating elements
% Call sample: mix([1,2],[3,4])
mix([], _) -> [];
mix([H1|T1], [H2|T2]) -> [H1|[H2|mix(T1,T2)]].
