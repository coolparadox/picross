-module(picross).
-export([test/0,
         solve/2]).

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
            RowSolvers = lists:map(fun({ Id, Fills }) -> solver:start_link(Id, length(Cols), Fills, row, self()) end, lists:zip(lists:seq(1, length(Rows)), Rows)),
            ColSolvers = lists:map(fun({ Id, Fills }) -> solver:start_link(Id, length(Rows), Fills, col, self()) end, lists:zip(lists:seq(1, length(Cols)), Cols)),
            lists:map(fun(Solver) -> solver:set_solvers(Solver, ColSolvers) end, RowSolvers),
            lists:map(fun(Solver) -> solver:set_solvers(Solver, RowSolvers) end, ColSolvers),
            case manage(RowSolvers ++ ColSolvers) of
                stalled -> ambiguous;
                nonsense -> invalid;
                { ok, SolversResult } -> { ok, lists:map(fun(Solver) -> maps:get(Solver, SolversResult) end, RowSolvers) }
            end
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
    lists:map(fun(Solver) -> solver:go(Solver) end, Solvers),
    Answer = manage(true, maps:from_list(lists:zip(Solvers, lists:duplicate(length(Solvers), working))), maps:new()),
    unregister(solverManager),
    Answer.

manage(IsGoodResult, SolversState, SolversResult) ->
    case lists:member(working, maps:values(SolversState)) of
        true ->
            receive
                { Solver, working } ->
                    manage(IsGoodResult, maps:put(Solver, working, SolversState), SolversResult);
                { Solver, stalled } ->
                    manage(IsGoodResult, maps:put(Solver, stalled, SolversState), SolversResult);
                { Solver, done, Result } ->
                    manage(IsGoodResult, maps:put(Solver, done, SolversState), maps:put(Solver, Result, SolversResult));
                { _, badhint } ->
                    manage(false, SolversState, SolversResult);
                Unexpected ->
                    exit("unexpected message", Unexpected)
            end;
        false ->
            lists:map(fun(Solver) -> solver:terminate(Solver) end, maps:keys(SolversState)),
            waitTermination(IsGoodResult, lists:member(stalled, maps:values(SolversState)), SolversState, SolversResult)
    end.

waitTermination(IsGoodResult, Stalled, SolversState, SolversResult) ->
    case lists:all(fun(State) -> case State of terminated -> true; _ -> false end end, maps:values(SolversState)) of
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
                { Solver, terminated } ->
                    waitTermination(IsGoodResult, Stalled, maps:put(Solver, terminated, SolversState), SolversResult);
                { _, badhint } ->
                    waitTermination(false, Stalled, SolversState, SolversResult);
                { _, done, _ } ->
                    waitTermination(IsGoodResult, Stalled, SolversState, SolversResult);
                Unexpected ->
                    exit("unexpected message", Unexpected)
            after 1000 ->
                      exit("termination timeout")
            end
    end.
