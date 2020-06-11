-module(picross).
-export([test/0,
         solve/2,
         map_to_str/1,
         str_to_map/1]).

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

    TestMap = fun(Str, Rows, Cols) ->
        Map = str_to_map(lists:filter(fun(Elem) -> Elem =/= $  end, Str)),
        { ok, Map } = solve(Rows, Cols)
    end,

    % 2x2 fill
    TestMap(
        "##
         ##
        ",
        [[2],
         [2]],
        [[2],
         [2]]),

    % 5x5 horse
    TestMap(
        "###..
         .#..#
         .####
         .###.
         .#.#.
        ",
        [[3],
         [1,1],
         [4],
         [3],
         [1,1]],
        [[1],
         [5],
         [1,2],
         [3],
         [2]]),

    % A non square puzzle
    TestMap(
        "###..##.##
         ###.#....#
         ####..####
         ##.#.#...#
        ",
        [[3,2,2],
         [3,1,1],
         [4,4],
         [2,1,1,1]],
        [[4],
         [4],
         [3],
         [2],
         [1],
         [1,1],
         [1,1],
         [1],
         [1,1],
         [4]]),

    % 15x15 duck
    TestMap(
        ".........###...
         ........#####..
         .......####.###
         .......#######.
         ........#####..
         .........###...
         ........#####..
         #.....########.
         ###..###...###.
         #######.###.##.
         .#####.####.##.
         .########..##..
         ..##########...
         ....##.###.....
         ......######...
        ",
        [[3],
         [5],
         [4,3],
         [7],
         [5],
         [3],
         [5],
         [1,8],
         [3,3,3],
         [7,3,2],
         [5,4,2],
         [8,2],
         [10],
         [2,3],
         [6]],
        [[3],
         [4],
         [5],
         [4],
         [5],
         [6],
         [3,2,1],
         [2,2,5],
         [4,2,6],
         [8,2,3],
         [8,2,1,1],
         [2,6,2,1],
         [4,6],
         [2,4],
         [1]]),

    % 25x25 owl
    TestMap(
        "###.....########......###
         .#######################.
         ...###...######...###....
         .#######..####..#######..
         .##...###..##..###..####.
         ##.....##..##.##......##.
         #...#.#..#.##.#..#.#...##
         #...####.##.###.####...##
         #.###..##.#..#.##..###..#
         #..#.##.#.#..#.#.##.#...#
         #.##.##.#.#..#.#.##.##..#
         #..##..##.#..#.##..##..##
         #...####.#..#.#.####...##
         ##..#.#.##.###.#.#.#..###
         .###...##.#####.#....##..
         ...######.#####.######...
         #...###....###..........#
         ###.........#..........##
         .###..................###
         ..###.###...##.####..##..
         ...#######.###########..#
         #.......#####.####......#
         #.####.............####.#
         #...#################...#
         ###...#####...#####...###
        ",
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

map_to_str(Maps) ->
    lists:foldl(fun(Map, Acc) -> Acc ++ picross_solver:map_to_str(Map) ++ "\n" end, "", Maps).

str_to_map(Str) ->
    lists:map(fun(S) -> picross_solver:str_to_map(S) end, lines(Str)).

lines(Str) -> lines(Str, "").

lines([$\n], LineAcc) -> [lists:reverse(LineAcc)];
lines([$\n|Str], LineAcc) -> [lists:reverse(LineAcc)|lines(Str, "")];
lines([Char|Str], LineAcc) -> lines(Str, [Char|LineAcc]).

solve(Rows, Cols) ->
    case check_inputs(Rows, Cols) of
        false -> badarg;
        true ->
            RowSolvers = lists:map(fun({ Id, Fills }) -> { ok, Pid } = picross_solver:start_link(Id, length(Cols), Fills, self()), Pid end, lists:zip(lists:seq(1, length(Rows)), Rows)),
            ColSolvers = lists:map(fun({ Id, Fills }) -> { ok, Pid } = picross_solver:start_link(Id, length(Rows), Fills, self()), Pid end, lists:zip(lists:seq(1, length(Cols)), Cols)),
            lists:foreach(fun(Solver) -> ok = picross_solver:prime(Solver, ColSolvers) end, RowSolvers),
            lists:foreach(fun(Solver) -> ok = picross_solver:prime(Solver, RowSolvers) end, ColSolvers),
            AllSolvers = RowSolvers ++ ColSolvers,
            Answer = case manage(AllSolvers) of
                stalled -> ambiguous;
                nonsense -> invalid;
                { ok, Results } -> { ok, lists:map(fun(Solver) -> maps:get(Solver, Results) end, RowSolvers) }
            end,
            lists:foreach(fun(Solver) -> ok = picross_solver:stop(Solver) end, AllSolvers),
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
    manage(true, maps:from_list(lists:zip(Solvers, lists:duplicate(length(Solvers), discovering)))).

manage(IsGoodResult, States) ->
    case lists:member(discovering, maps:values(States)) of
        true ->
            receive
                { Solver, discovering } ->
                    manage(IsGoodResult, maps:put(Solver, discovering, States));
                { Solver, stalled } ->
                    manage(IsGoodResult, maps:put(Solver, stalled, States));
                { Solver, resting } ->
                    manage(IsGoodResult, maps:put(Solver, resting, States));
                { _, badhint } ->
                    manage(false, States);
                Unexpected ->
                    exit("unexpected message", Unexpected)
            end;
        false ->
            case IsGoodResult of
                false -> nonsense;
                true -> waitTermination(true, lists:member(stalled, maps:values(States)), States, retireSolvers(maps:keys(States), maps:new()))
            end
    end.

retireSolvers([], Results) -> Results;
retireSolvers([Solver|Solvers], Results) ->
    retireSolvers(Solvers, maps:put(Solver, picross_solver:retire(Solver), Results)).

waitTermination(IsGoodResult, IsStalled, States, Results) ->
    io:format("waitTermination~n"),
    case lists:all(fun(State) -> case State of retired -> true; _ -> false end end, maps:values(States)) of
        true ->
            case IsGoodResult of
                false -> nonsense;
                true ->
                    case IsStalled of
                        true -> stalled;
                        false -> { ok, Results }
                    end
            end;
        false ->
            receive
                { Solver, retired } ->
                    io:format("retired~n"),
                    waitTermination(IsGoodResult, IsStalled, maps:put(Solver, retired, States), Results);
                { _, badhint } ->
                    io:format("badhint~n"),
                    waitTermination(false, IsStalled, States, Results);
%                { _, resting, _ } ->
%                    waitTermination(IsGoodResult, IsStalled, States, Results);
                Unexpected ->
                    exit("unexpected message", Unexpected)
            after 1000 ->
                      exit("termination timeout")
            end
    end.
