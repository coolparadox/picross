-module(picross).
-compile(export_all).

test() ->

    badarg = solve([], [[1]]),
    badarg = solve([[]], [[1]]),
    badarg = solve([[0]], [[1]]),
    badarg = solve([[1]], [[1, qwerty]]),
    badarg = solve([[1]], [[-1]]),
    badarg = solve([[1]], [[2]]),
    badarg = solve([[1]], [1]),
    { ok, [[fill]]} = solve([[1]], [[1]]),

    ambiguous = solve([[1], [1]], [[1], [1]]),
    invalid = solve([[1], [2]], [[2], [2]]),
    invalid = solve([[2], [1]], [[2], [2]]),
    invalid = solve([[2], [2]], [[1], [2]]),
    invalid = solve([[2], [2]], [[2], [1]]),
    { ok,
      [[fill, fill],
       [fill, fill]] } =
    solve([[2], [2]], [[2], [2]]),

    % 5x5 horse
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

    % A non square puzzle
    % ###..##.##
    % ###.#....#
    % ####..####
    % ##.#.#...#
    { ok,
      [[fill, fill, fill, gap, gap, fill, fill, gap, fill, fill],
       [fill, fill, fill, gap, fill, gap, gap, gap, gap, fill],
       [fill, fill, fill, fill, gap, gap, fill, fill, fill, fill],
       [fill, fill, gap, fill, gap, fill, gap, gap, gap, fill]] } =
    solve([[3,2,2],[3,1,1],[4,4],[2,1,1,1]], [[4],[4],[3],[2],[1],[1,1],[1,1],[1],[1,1],[4]]),

    % 15x15 duck
    % .........###...
    % ........#####..
    % .......####.###
    % .......#######.
    % ........#####..
    % .........###...
    % ........#####..
    % #.....########.
    % ###..###...###.
    % #######.###.##.
    % .#####.####.##.
    % .########..##..
    % ..##########...
    % ....##.###.....
    % ......######...
    { ok,
      [[gap, gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, gap, gap, gap],
       [gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, gap, gap],
       [gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, gap, fill, fill, fill],
       [gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, fill, fill, gap],
       [gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, gap, gap],
       [gap, gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, gap, gap, gap],
       [gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, gap, gap],
       [fill, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, fill, fill, fill, gap],
       [fill, fill, fill, gap, gap, fill, fill, fill, gap, gap, gap, fill, fill, fill, gap],
       [fill, fill, fill, fill, fill, fill, fill, gap, fill, fill, fill, gap, fill, fill, gap],
       [gap, fill, fill, fill, fill, fill, gap, fill, fill, fill, fill, gap, fill, fill, gap],
       [gap, fill, fill, fill, fill, fill, fill, fill, fill, gap, gap, fill, fill, gap, gap],
       [gap, gap, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, gap, gap, gap],
       [gap, gap, gap, gap, fill, fill, gap, fill, fill, fill, gap, gap, gap, gap, gap],
       [gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, fill, gap, gap, gap]] } =
    solve(
      [[3],[5],[4,3],[7],[5],[3],[5],[1,8],[3,3,3],[7,3,2],[5,4,2],[8,2],[10],[2,3],[6]],
      [[3],[4],[5],[4],[5],[6],[3,2,1],[2,2,5],[4,2,6],[8,2,3],[8,2,1,1],[2,6,2,1],[4,6],[2,4],[1]]),

    % 25x25 owl
    % ###.....########......###
    % .#######################.
    % ...###...######...###....
    % .#######..####..#######..
    % .##...###..##..###..####.
    % ##.....##..##.##......##.
    % #...#.#..#.##.#..#.#...##
    % #...####.##.###.####...##
    % #.###..##.#..#.##..###..#
    % #..#.##.#.#..#.#.##.#...#
    % #.##.##.#.#..#.#.##.##..#
    % #..##..##.#..#.##..##..##
    % #...####.#..#.#.####...##
    % ##..#.#.##.###.#.#.#..###
    % .###...##.#####.#....##..
    % ...######.#####.######...
    % #...###....###..........#
    % ###.........#..........##
    % .###..................###
    % ..###.###...##.####..##..
    % ...#######.###########..#
    % #.......#####.####......#
    % #.####.............####.#
    % #...#################...#
    % ###...#####...#####...###
    { ok,
      [[fill, fill, fill, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, fill, fill, fill, gap, gap, gap, gap, gap, gap, fill, fill, fill],
       [gap, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, gap],
       [gap, gap, gap, fill, fill, fill, gap, gap, gap, fill, fill, fill, fill, fill, fill, gap, gap, gap, fill, fill, fill, gap, gap, gap, gap],
       [gap, fill, fill, fill, fill, fill, fill, fill, gap, gap, fill, fill, fill, fill, gap, gap, fill, fill, fill, fill, fill, fill, fill, gap, gap],
       [gap, fill, fill, gap, gap, gap, fill, fill, fill, gap, gap, fill, fill, gap, gap, fill, fill, fill, gap, gap, fill, fill, fill, fill, gap],
       [fill, fill, gap, gap, gap, gap, gap, fill, fill, gap, gap, fill, fill, gap, fill, fill, gap, gap, gap, gap, gap, gap, fill, fill, gap],
       [fill, gap, gap, gap, fill, gap, fill, gap, gap, fill, gap, fill, fill, gap, fill, gap, gap, fill, gap, fill, gap, gap, gap, fill, fill],
       [fill, gap, gap, gap, fill, fill, fill, fill, gap, fill, fill, gap, fill, fill, fill, gap, fill, fill, fill, fill, gap, gap, gap, fill, fill],
       [fill, gap, fill, fill, fill, gap, gap, fill, fill, gap, fill, gap, gap, fill, gap, fill, fill, gap, gap, fill, fill, fill, gap, gap, fill],
       [fill, gap, gap, fill, gap, fill, fill, gap, fill, gap, fill, gap, gap, fill, gap, fill, gap, fill, fill, gap, fill, gap, gap, gap, fill],
       [fill, gap, fill, fill, gap, fill, fill, gap, fill, gap, fill, gap, gap, fill, gap, fill, gap, fill, fill, gap, fill, fill, gap, gap, fill],
       [fill, gap, gap, fill, fill, gap, gap, fill, fill, gap, fill, gap, gap, fill, gap, fill, fill, gap, gap, fill, fill, gap, gap, fill, fill],
       [fill, gap, gap, gap, fill, fill, fill, fill, gap, fill, gap, gap, fill, gap, fill, gap, fill, fill, fill, fill, gap, gap, gap, fill, fill],
       [fill, fill, gap, gap, fill, gap, fill, gap, fill, fill, gap, fill, fill, fill, gap, fill, gap, fill, gap, fill, gap, gap, fill, fill, fill],
       [gap, fill, fill, fill, gap, gap, gap, fill, fill, gap, fill, fill, fill, fill, fill, gap, fill, gap, gap, gap, gap, fill, fill, gap, gap],
       [gap, gap, gap, fill, fill, fill, fill, fill, fill, gap, fill, fill, fill, fill, fill, gap, fill, fill, fill, fill, fill, fill, gap, gap, gap],
       [fill, gap, gap, gap, fill, fill, fill, gap, gap, gap, gap, fill, fill, fill, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, fill],
       [fill, fill, fill, gap, gap, gap, gap, gap, gap, gap, gap, gap, fill, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, fill, fill],
       [gap, fill, fill, fill, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill],
       [gap, gap, fill, fill, fill, gap, fill, fill, fill, gap, gap, gap, fill, fill, gap, fill, fill, fill, fill, gap, gap, fill, fill, gap, gap],
       [gap, gap, gap, fill, fill, fill, fill, fill, fill, fill, gap, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, gap, gap, fill],
       [fill, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, fill, gap, fill, fill, fill, fill, gap, gap, gap, gap, gap, gap, fill],
       [fill, gap, fill, fill, fill, fill, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, gap, fill, fill, fill, fill, gap, fill],
       [fill, gap, gap, gap, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, fill, gap, gap, gap, fill],
       [fill, fill, fill, gap, gap, gap, fill, fill, fill, fill, fill, gap, gap, gap, fill, fill, fill, fill, fill, gap, gap, gap, fill, fill, fill]] } =
    solve(
      [[3,8,3],
       [23],
       [3,6,3],
       [7,4,7],
       [2,3,2,3,4],
       [2,2,2,2,2],
       [1,1,1,1,2,1,1,1,2],
       [1,4,2,3,4,2],
       [1,3,2,1,1,2,3,1],
       [1,1,2,1,1,1,1,2,1,1],
       [1,2,2,1,1,1,1,2,2,1],
       [1,2,2,1,1,2,2,2],
       [1,4,1,1,1,4,2],
       [2,1,1,2,3,1,1,1,3],
       [3,2,5,1,2],
       [6,5,6],
       [1,3,3,1],
       [3,1,2],
       [3,3],
       [3,3,2,4,2],
       [7,11,1],
       [1,5,4,1],
       [1,4,4,1],
       [1,17,1],
       [3,5,5,3]],
      [[1,9,2,4],
       [2,3,2,2,1],
       [2,2,1,1,1,3,1,1],
       [3,4,2,3,1],
       [3,3,3,2,2,2],
       [3,1,2,1,2,1,2],
       [1,2,2,2,2,2,2,2],
       [1,3,2,2,2,2,2],
       [2,2,4,3,3,2],
       [3,2,2,2,2],
       [4,5,2,1,2],
       [7,4,2,1],
       [8,6,3,1],
       [4,5,4,2,1],
       [3,3,1,2,2,2],
       [2,2,4,1,3,2],
       [1,2,2,2,2,3,2],
       [1,2,2,2,2,1,3,2],
       [3,1,2,1,1,2,2],
       [3,3,3,1,1,2],
       [4,4,1,1,2],
       [1,2,1,1,2,2,1],
       [2,3,2,2,1,1],
       [2,4,3,2,1],
       [1,8,3,5]]),

    ok.

printMap([]) -> ok;
printMap([Row|Rows]) ->
    printRow(Row),
    printMap(Rows).

printRow([]) ->
    io:format("~n");
printRow([fill|Marks]) ->
    io:format("#"),
    printRow(Marks);
printRow([gap|Marks]) ->
    io:format("."),
    printRow(Marks).

solve(Rows, Cols) ->
    case check_inputs(Rows, Cols) of
        false -> badarg;
        true ->
            RowSolvers = lists:map(fun({ Id, Fills }) -> spawn_link(?MODULE, solver, [Id, length(Cols), Fills, row]) end, lists:zip(lists:seq(1, length(Rows)), Rows)),
            ColSolvers = lists:map(fun({ Id, Fills }) -> spawn_link(?MODULE, solver, [Id, length(Rows), Fills, col]) end, lists:zip(lists:seq(1, length(Cols)), Cols)),
            lists:map(fun(Pid) -> Pid ! { solvers, ColSolvers } end, RowSolvers),
            lists:map(fun(Pid) -> Pid ! { solvers, RowSolvers } end, ColSolvers),
            Answer = case manage(RowSolvers ++ ColSolvers) of
                stalled -> ambiguous;
                nonsense -> invalid;
                { ok, SolversResult } -> { ok, lists:map(fun(Solver) -> maps:get(Solver, SolversResult) end, RowSolvers) }
            end,
            lists:map(fun(Pid) -> Pid ! terminate end, RowSolvers ++ ColSolvers),
            Answer
    end.

check_inputs([], _) -> false;
check_inputs(_, []) -> false;
check_inputs(Rows, Cols) ->
    is_list(Rows)
    andalso is_list(Cols)
    andalso check_inputs2(Rows, length(Cols))
    andalso check_inputs2(Cols, length(Rows)).

check_inputs2([], _) -> false;
check_inputs2(Fills, Max) ->
    is_list(Fills)
    andalso lists:all(fun(Fill) -> check_inputs3(Fill, Max) end, Fills).

check_inputs3([], _) -> false;
check_inputs3(Fill, Max) ->
    is_list(Fill)
    andalso lists:all(fun(Val) -> Val > 0 andalso Val =< Max end, Fill).

manage(Solvers) ->
    register(solverManager, self()),
    lists:map(fun(Pid) -> Pid ! go end, Solvers),
    Answer = manage(true, maps:from_list(lists:zip(Solvers, lists:duplicate(length(Solvers), work))), maps:new()),
    unregister(solverManager),
    Answer.

manage(IsGoodResult, SolversState, SolversResult) ->
    case lists:member(work, maps:values(SolversState)) of
        true ->
            receive
                { Solver, work } ->
                    %io:format("~w work~n", [Solver]),
                    manage(IsGoodResult, maps:put(Solver, work, SolversState), SolversResult);
                { Solver, wait } ->
                    %io:format("~w wait~n", [Solver]),
                    manage(IsGoodResult, maps:put(Solver, wait, SolversState), SolversResult);
                { Solver, done, Result } ->
                    %io:format("~w done~n", [Solver]),
                    manage(IsGoodResult, maps:put(Solver, done, SolversState), maps:put(Solver, Result, SolversResult));
                { _, badhint } ->
                    %io:format("~w bad hint~n", [Solver]),
                    manage(false, SolversState, SolversResult);
                Unexpected ->
                    exit("unexpected message", Unexpected)
            end;
        false ->
            lists:map(fun(Pid) -> Pid ! terminate end, maps:keys(SolversState)),
            waitTermination(IsGoodResult, lists:member(wait, maps:values(SolversState)), SolversState, SolversResult)
    end.

waitTermination(IsGoodResult, Stalled, SolversState, SolversResult) ->
    case lists:all(fun(State) -> case State of terminate -> true; _ -> false end end, maps:values(SolversState)) of
        true ->
            case IsGoodResult of
                false -> nonsense;
                true ->
                    case Stalled of
                        true -> stalled;
                        false -> { ok, SolversResult }
                    end
            end;
        false ->
            receive
                { Solver, terminate } ->
                    %io:format("~w terminate~n", [Solver]),
                    waitTermination(IsGoodResult, Stalled, maps:put(Solver, terminate, SolversState), SolversResult);
                { _, badhint } ->
                    %io:format("~w bad hint~n", [Solver]),
                    waitTermination(false, Stalled, SolversState, SolversResult);
                { _, done, _ } ->
                    %io:format("~w done~n", [Solver]),
                    waitTermination(IsGoodResult, Stalled, SolversState, SolversResult);
                Unexpected ->
                    exit("unexpected message", Unexpected)
            after 1000 ->
                      exit("termination timeout")
            end
    end.

solver(Id, Length, Fills, Tag) ->
    solver(work, Id, Length, Fills, Tag, [], emergeClue(mapCombine(Fills, Length))).

solver(work, Id, Length, Fills, Tag, TransposedSolvers, Clue) ->
    receive
        { solvers, Solvers } ->
            %io:format("~w ~B work: solvers~n", [Tag, Id]),
            solver(work, Id, Length, Fills, Tag, Solvers, Clue);
        go ->
            %io:format("~w ~B work: go~n", [Tag, Id]),
            hintSolvers(Id, TransposedSolvers, Clue),
            case lists:member(unknown, Clue) of
                true ->
                    solver(work, Id, Length, Fills, Tag, TransposedSolvers, Clue);
                false ->
                    solver(done, Id, Length, Fills, Tag, TransposedSolvers, Clue)
            end;
        { hint, Position, Hint } ->
            %io:format("~w ~B work: hint ~B ~w~n", [Tag, Id, Position, Hint]),
            solverManager ! { self(), work },
            { HintQuality, HintedClue } = acknowledgeHint(Clue, Position, Hint),
            case HintQuality of
                nonsense ->
                    solverManager ! { self(), badhint },
                    solver(work, Id, Length, Fills, Tag, TransposedSolvers, Clue);
                known ->
                    solver(work, Id, Length, Fills, Tag, TransposedSolvers, Clue);
                useful ->
                    NewClue = emergeClue(HintedClue, mapCombine(Fills, Length)),
                    hintSolvers(Id, TransposedSolvers, emergeHints(Clue, NewClue)),
                    case lists:member(unknown, NewClue) of
                        true ->
                            solver(work, Id, Length, Fills, Tag, TransposedSolvers, NewClue);
                        false ->
                            solver(done, Id, Length, Fills, Tag, TransposedSolvers, NewClue)
                    end
            end;
        terminate ->
            %io:format("~w ~B work: terminate~n", [Tag, Id]),
            solverManager ! { self(), terminate };
        Unexpected ->
            exit("unexpected message", Unexpected)
    after 100 ->
        solverManager ! { self(), wait },
        solver(work, Id, Length, Fills, Tag, TransposedSolvers, Clue)
    end;
solver(done, Id, Length, Fills, Tag, TransposedSolvers, Clue) ->
    solverManager ! { self(), done, Clue },
    receive
        { hint, Position, Hint } ->
            %io:format("~w ~B done: hint ~B ~w~n", [Tag, Id, Position, Hint]),
            { HintQuality, _ } = acknowledgeHint(Clue, Position, Hint),
            case HintQuality of
                nonsense ->
                    solverManager ! { self(), badhint },
                    solver(done, Id, Length, Fills, Tag, TransposedSolvers, Clue);
                known ->
                    solver(done, Id, Length, Fills, Tag, TransposedSolvers, Clue)
            end;
        terminate ->
            %io:format("~w ~B done: terminate~n", [Tag, Id]),
            solverManager ! { self(), terminate };
        Unexpected ->
            exit("unexpected message", Unexpected)
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
            { useful, Before ++ [Hint|After] };
        { fill, fill } ->
            { known, [] };
        { gap, gap } ->
            { known, [] };
        _ ->
            { nonsense, [] }
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
