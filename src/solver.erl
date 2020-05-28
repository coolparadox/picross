-module(solver).
-export([test/0,
         start/5,
         start_link/5,
         set_solvers/2,
         go/1,
         terminate/1,
         init/5,
         str_to_map/1,
         map_to_str/1]).
-record(state, { tag, id, length, fills, clue, manager, solvers=[], stalled=false }).
-define(StallTimeout, 100).

str_to_map(Str) ->
    lists:map(
        fun(Char) ->
            case Char of
                $# -> fill;
                $. -> gap;
                $? -> unknown
            end
        end, Str).

map_to_str(Map) ->
    lists:map(
        fun(Mark) ->
            case Mark of
                fill -> $#;
                gap -> $.;
                unknown -> $?
            end
        end, Map).

test() ->

    [fill, gap, unknown] = str_to_map("#.?"),
    "#.?" = map_to_str([fill, gap, unknown]),

    StartMessageForwarder = fun(Tag, Pid) ->
        spawn_link(fun Forwarder() ->
            receive
                Message ->
                    Pid ! { Tag, Message },
                    Forwarder()
                after 10 * ?StallTimeout -> ok
            end
        end)
    end,

    GetNextMessage = fun() ->
        receive
            Message -> Message
        after 2 * ?StallTimeout ->
                  error(timeout)
        end
    end,

    % Solve '1' in the first row of a puzzle with 1 column.
    Solver1 = start_link(1, 1, [1], tag, self()),
    set_solvers(Solver1, [StartMessageForwarder(1, self())]),
    go(Solver1),
    % This puzzle can be solved immediately.
    { Solver1, done, [fill] } = GetNextMessage(),
    % Also the solver of the first column should be hinted that its first position is filled.
    { 1, { hint, 1, fill } } = GetNextMessage(),
    % Verify termination.
    terminate(Solver1),
    { Solver1, terminated } = GetNextMessage(),

    % Same as above, but the solver receives a nonsense hint.
    Solver1a = start_link(1, 1, [1], tag, self()),
    set_solvers(Solver1a, [StartMessageForwarder(1, self())]),
    go(Solver1a),
    { Solver1a, done, [fill] } = GetNextMessage(),
    { 1, { hint, 1, fill } } = GetNextMessage(),
    % Simulate the solver of the column hinting an absurd information.
    % The bad hint should be acknowledged.
    Solver1a ! { hint, 1, gap },
    { Solver1a, badhint } = GetNextMessage(),
    { Solver1a, done, [fill] } = GetNextMessage(),
    % Verify termination.
    terminate(Solver1a),
    { Solver1a, terminated } = GetNextMessage(),

    % Solve '2' in the fifth row of a puzzle with 3 columns.
    Solver2 = start_link(5, 3, [2], tag, self()),
    set_solvers(Solver2, lists:map(fun(Id) -> StartMessageForwarder(Id, self()) end, [1, 2, 3])),
    go(Solver2),
    % The initial information is only enough to determine that the middle position is filled, ie: '?#?'
    % Therefore the solver of the second column should be hinted that its fifth position is a fill.
    { 2, { hint, 5, fill } } = GetNextMessage(),
    % No further work is possible with the available information; check for stall detection.
    { Solver2, stalled } = GetNextMessage(),
    % Let's simulate receiving a hint from the solver of the first column.
    Solver2 ! { hint, 1, gap },
    % This is a useful information that leads to the solution '.##'
    % and subsequently hinting the remaining solver of the third column.
    { Solver2, working } = GetNextMessage(),
    { Solver2, done, [gap, fill, fill] } = GetNextMessage(),
    { 3, { hint, 5, fill } } = GetNextMessage(),
    % Verify termination.
    terminate(Solver2),
    { Solver2, terminated } = GetNextMessage(),

    ok.

start(Id, Length, Fills, Tag, Manager) ->
    spawn(?MODULE, init, [Id, Length, Fills, Tag, Manager]).

start_link(Id, Length, Fills, Tag, Manager) ->
    spawn_link(?MODULE, init, [Id, Length, Fills, Tag, Manager]).

init(Id, Length, Fills, Tag, Manager) ->
    case check_inputs(Fills, Length) of
        false -> badarg;
        true ->
            solver(working, #state{tag=Tag, id=Id, length=Length, fills=Fills, clue=emerge_clue(map_combine(Fills, Length)), manager=Manager})
    end.

set_solvers(Solver, Solvers) ->
    Solver ! { solvers, Solvers }.

go(Solver) ->
    Solver ! go.

terminate(Solver) ->
    Solver ! terminate.

check_inputs([], _) -> false;
check_inputs(Fills, Max) ->
    is_list(Fills)
    andalso lists:all(fun(Val) -> Val > 0 andalso Val =< Max end, Fills).

solver(working, S) ->
    receive
        { solvers, Solvers } ->
            solver(working, S#state{solvers=Solvers, stalled=false});
        go ->
            hint_solvers(S#state.id, S#state.solvers, S#state.clue),
            case lists:member(unknown, S#state.clue) of
                true ->
                    solver(working, S#state{stalled=false});
                false ->
                    solver(done, S#state{stalled=false})
            end;
        { hint, Position, Hint } ->
            case S#state.stalled of
                true -> S#state.manager ! { self(), working };
                false -> ok
            end,
            { HintQuality, HintedClue } = acknowledge_hint(S#state.clue, Position, Hint),
            case HintQuality of
                nonsense ->
                    S#state.manager ! { self(), badhint },
                    solver(working, S#state{stalled=false});
                known ->
                    solver(working, S#state{stalled=false});
                useful ->
                    NewClue = emerge_clue(HintedClue, map_combine(S#state.fills, S#state.length)),
                    hint_solvers(S#state.id, S#state.solvers, emerge_hints(HintedClue, NewClue)),
                    case lists:member(unknown, NewClue) of
                        true ->
                            solver(working, S#state{clue=NewClue, stalled=false});
                        false ->
                            solver(done, S#state{clue=NewClue, stalled=false})
                    end
            end;
        terminate ->
            S#state.manager ! { self(), terminated };
        Unexpected ->
            error(unexpected, [Unexpected])
    after case S#state.stalled of true -> infinity; false -> ?StallTimeout end ->
        S#state.manager ! { self(), stalled },
        solver(working, S#state{stalled=true})
    end;
solver(done, S) ->
    S#state.manager ! { self(), done, S#state.clue },
    receive
        { hint, Position, Hint } ->
            { HintQuality, _ } = acknowledge_hint(S#state.clue, Position, Hint),
            case HintQuality of
                nonsense ->
                    S#state.manager ! { self(), badhint },
                    solver(done, S);
                known ->
                    solver(done, S)
            end;
        terminate ->
            S#state.manager ! { self(), terminated };
        Unexpected ->
            error(unexpected, [Unexpected])
    end.

emerge_hints([], []) -> [];
emerge_hints([unknown|OldHints], [NewHint|NewHints]) ->
    [NewHint|emerge_hints(OldHints, NewHints)];
emerge_hints([_|OldHints], [_|NewHints]) ->
    [unknown|emerge_hints(OldHints, NewHints)].

acknowledge_hint(Clue, Position, Hint) ->
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

hint_solvers(_, [], []) -> ok;
hint_solvers(Position, [_|Solvers], [unknown|Hints]) ->
    hint_solvers(Position, Solvers, Hints);
hint_solvers(Position, [Solver|Solvers], [Hint|Hints]) ->
    Solver ! { hint, Position, Hint },
    hint_solvers(Position, Solvers, Hints).

map_combine(Fills, Length) -> lists:map(fun picr_to_map/1, picr_combine(Fills, Length)).

% Maps = lists:map(fun picross:picr_to_map/1, picross:picr_combine([8], 10)).
% FirstClue = picross:emerge_clue(Maps).
% NewClue = picross:emerge_clue(OldClue, Maps).

emerge_clue([Map|Maps]) -> emerge_clue(lists:duplicate(length(Map), unknown), [Map|Maps]).

emerge_clue(Clue, [Map|Maps]) ->
    case match_map_clue(Map, Clue) of
        false -> emerge_clue(Clue, Maps);
        true -> emerge_clue(Clue, Map, Maps)
    end.

emerge_clue(_, Result, []) -> Result;
emerge_clue(Clue, Result, [Map|Maps]) ->
    emerge_clue(
      Clue,
      case match_map_clue(Map, Clue) of
          true->update_clue(Result, Map);
          _ -> Result
      end,
      Maps).

match_map_clue([], []) -> true;
match_map_clue([_|MapT], [unknown|ClueT]) -> match_map_clue(MapT, ClueT);
match_map_clue([gap|MapT], [gap|ClueT]) -> match_map_clue(MapT, ClueT);
match_map_clue([fill|MapT], [fill|ClueT]) -> match_map_clue(MapT, ClueT);
match_map_clue([_|_], [_|_]) -> false.

% update_clue([gap,gap,fill,fill],[gap,fill,gap,fill]) -> [gap,unknown,unknown,fill]
update_clue(Reference, Map) ->
    lists:foldr(
        fun({R,M}, Acc) ->
            [case {R,M} of
                {gap,gap} -> gap;
                {fill,fill} -> fill;
                _ -> unknown
            end | Acc]
        end,
        [],
        lists:zip(Reference, Map)
    ).

% picr_to_map([1,2,3,3,1]) -> [gap,fill,fill,gap,gap,gap,fill,fill,fill,gap]
picr_to_map(Picr) -> picr_to_map(Picr, gap, []).
picr_to_map([], _, Acc) -> lists:reverse(Acc);
picr_to_map([0|Tail], gap, Acc) -> picr_to_map(Tail, fill, Acc);
picr_to_map([0|Tail], fill, Acc) -> picr_to_map(Tail, gap, Acc);
picr_to_map([N|Tail], gap, Acc) -> picr_to_map([N-1|Tail], gap, [gap|Acc]);
picr_to_map([N|Tail], fill, Acc) -> picr_to_map([N-1|Tail], fill, [fill|Acc]).

% Calculate all combinations of a line or column of a picross puzzle.
% Parameters:
% - Fills: list of sizes of filled regions
% - Length: length of the line or column
% Each element of the answer is a list where the first element is the size of the first gap,
% followed by the size of the first region, next gap, next region and so on.
% Call sample: picr_combine([2,3], 10)
picr_combine([], _) -> [];
picr_combine(Fills, Length) ->
    % io:format("~w: picr_combine(~w, ~B)~n", [self(), Fills, Length]),
    [ [FirstGap|blend(Fills, Gaps)] || [FirstGap|Gaps] <- xfill(Length - lists:sum(Fills), length(Fills) + 1) ].

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

% Discover all combinations of integer lists where:
% - The number of items is 'Count'
% - Each element is equal or greater than 1
% - The sum of all elements is 'Sum'
% Example: hfill(5,3) -> [[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1]]
hfill(Sum, 1) -> [[Sum]];
hfill(Sum, Count) when Count > 1 andalso Sum >= Count ->
    [ [H|T] || H <- lists:seq(1,Sum-(Count-1)), T <- hfill(Sum-H, Count-1) ].

% Blend two lists of equal size, alternating elements
% Example: blend([1,2],[3,4]) -> [1,3,2,4]
blend(L1,L2) ->
    lists:foldr(
        fun({H1,H2}, Acc) ->
            [H1|[H2|Acc]]
        end,
        [],
        lists:zip(L1, L2)
     ).
