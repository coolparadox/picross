-module(picross).
-compile(export_all).

test() ->

    % horse
    % ###..
    % .#..#
    % .####
    % .###.
    % .#.#.
    { ok,
      [[fill, fill, fill, gap, gap],
       [gap, fill, gap, gap, fill],
       [gap, fill, fill, fill, fill],
       [gap, fill, fill, fill, gap],
       [gap, fill, gap, fill, gap]] } =
    solve([[3], [1,1], [4], [3], [1,1]], [[1], [5], [1,2], [3], [2]]),

    % dubious
    % #.
    % .#
    { ambiguous, _ } = solve([[1], [1]], [[1], [1]]).

solve(Rows, Cols) ->
    RowSolvers = lists:map(fun({ Id, Fills }) -> spawn_link(?MODULE, solver, [Id, length(Cols), Fills, row]) end, lists:zip(lists:seq(1, length(Rows)), Rows)),
    ColSolvers = lists:map(fun({ Id, Fills }) -> spawn_link(?MODULE, solver, [Id, length(Rows), Fills, col]) end, lists:zip(lists:seq(1, length(Cols)), Cols)),
    lists:map(fun(Pid) -> Pid ! { solvers, ColSolvers } end, RowSolvers),
    lists:map(fun(Pid) -> Pid ! { solvers, RowSolvers } end, ColSolvers),
    register(solversListener, self()),
    Answer = listen(RowSolvers ++ ColSolvers),
    unregister(solversListener),
    Answer.

%watcher(ReportPid, SolverStates) ->
%    receive
%        { Solver, ok } ->
%            watcher(ReportPid, maps:put(Solver, ok, SolverStates));
%        { Solver, done } ->
%            watcher(ReportPid, maps:remove(Solver, SolverStates));
%        { Solver, stalled } ->
%            NewStates = maps:put(Solver, stalled, SolverStates),
%            case lists:member(ok, maps:values(NewStates)) of
%                true ->
%                    watcher(ReportPid, NewStates);
%                false ->
%                    ReportPid ! stalled,
%                    watcher(ReportPid, NewStates)
%            end;
%        terminate ->
%            lists:map(fun(Pid) -> Pid ! terminate end, maps:keys(SolverStates)),
%            ok;
%        Unexpected ->
%            exit({"unexpected message", Unexpected})
%    end.
%
%wait([], Result) -> { ok, Result };
%wait(RemainingSolvers, Result) ->
%    receive
%        { From, FillMap } ->
%            wait(lists:delete(From, RemainingSolvers), maps:put(From, FillMap, Result));
%        stalled ->
%            stalled;
%        Unexpected ->
%            exit({"unexpected message", Unexpected})
%    end.

listen(Solvers) ->
    listen2(maps:from_list(lists:zip(Solvers, lists:duplicate(length(Solvers), working)))).

listen2(SolversState) ->
    receive
        Unexpected ->
            exit({ "unexpected message", Unexpected })
    end.

solver(Id, Length, Fills, Tag) ->
    solver(Id, Length, Fills, Tag, [], emergeClue(mapCombine(Fills, Length))).

solver(Id, Length, Fills, Tag, TransposedSolvers, Clue) ->
    receive
        { solvers, Solvers } ->
            solver(Id, Length, Fills, Tag, Solvers, Clue);
        { hint, Position, Hint } ->
            { IsNewHint, HintedClue } = acknowledgeHint(Clue, Position, Hint),
            case IsNewHint of
                false -> solver(Id, Length, Fills, Tag, TransposedSolvers, Clue);
                true ->
                    solversListener ! { self(), working },
                    NewClue = emergeClue(HintedClue, mapCombine(Fills, Length)),
                    hintSolvers(Id, TransposedSolvers, emergeHints(Clue, NewClue)),
                    case lists:member(unknown, NewClue) of
                        true ->
                            solver(Id, Length, Fills, Tag, TransposedSolvers, NewClue);
                        false ->
                            solversListener ! { self(), done, Tag, Id, NewClue }
                    end
            end;
        go ->
            hintSolvers(Id, TransposedSolvers, Clue),
            solver(Id, Length, Fills, Tag, TransposedSolvers, Clue);
        Unexpected ->
            exit({ "unexpected message", Unexpected })
    after 500 ->
        solversListener ! { self(), stalled },
        solver(Id, Length, Fills, Tag, TransposedSolvers, Clue)
    end.

emergeHints([], []) -> [];
emergeHints([unknown|OldHints], [NewHint|NewHints]) ->
    [NewHint|emergeHints(OldHints, NewHints)];
emergeHints([_|OldHints], [_|NewHints]) ->
    [unknown|emergeHints(OldHints, NewHints)].

acknowledgeHint(Clue, Position, Hint) ->
    case { lists:nth(Position, Clue), Hint } of
        { unknown, _ } ->
            {Before, [_|After]} = lists:split(Position - 1, Clue),
            { true, Before ++ [Hint|After] };
        { fill, fill } ->
            { false, [] };
        { gap, gap } ->
            { false, [] }
    end.

hintSolvers(_, [], []) -> ok;
hintSolvers(Position, [_|Solvers], [unknown|Hints]) ->
    hintSolvers(Position, Solvers, Hints);
hintSolvers(Position, [Solver|Solvers], [Hint|Hints]) ->
    Solver ! { hint, Position, Hint },
    hintSolvers(Position, Solvers, Hints).

mapCombine(Fills, Length) -> lists:map(fun picr2map/1, picrCombine(Fills, Length)).

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
