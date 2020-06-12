-module(picross_solver).
-behaviour(gen_statem).
-export([test/0]).
-export([str_to_map/1, map_to_str/1]).
-export([start/4, start_link/4, stop/1, prime/2, retire/1, get_result/1]).
-export([callback_mode/0, init/1]).
-export([priming/3, discovering/3, stalled/3, resting/3, retired/3]).
-record(state, { id, length, fills, listener, solvers=[], clue=[] }).
-define(StallTimeout, 100).

% Tests

test() ->

    [fill, gap, unknown] = str_to_map("  #.?  "),
    "#.?" = map_to_str([fill, gap, unknown]),

    GetNextMessage = fun() ->
        receive
            Message -> Message
        after 2 * ?StallTimeout ->
                  error(timeout)
        end
    end,

    % Solve a filled 1x1 puzzle.
    ok = fun() ->

        { ok, RowSolver } = start_link(1, 1, [1], self()),
        { RowSolver, state, priming } = GetNextMessage(),

        { ok, ColSolver } = start_link(1, 1, [1], self()),
        { ColSolver, state, priming } = GetNextMessage(),

        % This puzzle is immediately solved; no hint is required.
        ok = prime(RowSolver, [ColSolver]),
        { RowSolver, state, resting } = GetNextMessage(),
        ok = prime(ColSolver, [RowSolver]),
        { ColSolver, state, resting } = GetNextMessage(),

        % Check solution.
        ok = retire(RowSolver),
        { RowSolver, state, retired } = GetNextMessage(),
        [fill] = get_result(RowSolver),
        ok = retire(ColSolver),
        { ColSolver, state, retired } = GetNextMessage(),
        [fill] = get_result(ColSolver),

        % Release resources.
        ok = stop(RowSolver),
        ok = stop(ColSolver),

        ok
    end(),

    % Same as above, but one solver receives a nonsense hint.
    ok = fun() ->

        { ok, RowSolver } = start_link(1, 1, [1], self()),
        { RowSolver, state, priming } = GetNextMessage(),
        { ok, ColSolver } = start_link(1, 1, [1], self()),
        { ColSolver, state, priming } = GetNextMessage(),
        ok = prime(RowSolver, [ColSolver]),
        { RowSolver, state, resting } = GetNextMessage(),
        ok = prime(ColSolver, [RowSolver]),
        { ColSolver, state, resting } = GetNextMessage(),

        % Check for acknowledgement of a bad hint.
        ok = hint(RowSolver, 1, gap),
        { RowSolver, badhint } = GetNextMessage(),

        ok = retire(RowSolver),
        { RowSolver, state, retired } = GetNextMessage(),
        ok = retire(ColSolver),
        { ColSolver, state, retired } = GetNextMessage(),
        ok = stop(RowSolver),
        ok = stop(ColSolver),

        ok
    end(),

    % Solve a puzzle with a unique solution:
    % ##
    % .#
    ok = fun() ->

        { ok, RowSolver1 } = start_link(1, 2, [2], self()),
        { RowSolver1, state, priming } = GetNextMessage(),
        { ok, RowSolver2 } = start_link(2, 2, [1], self()),
        { RowSolver2, state, priming } = GetNextMessage(),
        { ok, ColSolver1 } = start_link(1, 2, [1], self()),
        { ColSolver1, state, priming } = GetNextMessage(),
        { ok, ColSolver2 } = start_link(2, 2, [2], self()),
        { ColSolver2, state, priming } = GetNextMessage(),

        ok = prime(RowSolver1, [ColSolver1, ColSolver2]),
        { RowSolver1, state, resting } = GetNextMessage(),
        ok = prime(RowSolver2, [ColSolver1, ColSolver2]),
        { RowSolver2, state, discovering } = GetNextMessage(),
        ok = prime(ColSolver1, [RowSolver1, RowSolver2]),
        { ColSolver1, state, discovering } = GetNextMessage(),
        { ColSolver1, state, resting } = GetNextMessage(),
        { RowSolver2, state, resting } = GetNextMessage(),
        ok = prime(ColSolver2, [RowSolver1, RowSolver2]),
        { ColSolver2, state, resting } = GetNextMessage(),

        ok = retire(RowSolver1),
        { RowSolver1, state, retired } = GetNextMessage(),
        [fill, fill] = get_result(RowSolver1),
        ok = retire(RowSolver2),
        { RowSolver2, state, retired } = GetNextMessage(),
        [gap, fill] = get_result(RowSolver2),
        ok = retire(ColSolver1),
        { ColSolver1, state, retired } = GetNextMessage(),
        [fill, gap] = get_result(ColSolver1),
        ok = retire(ColSolver2),
        { ColSolver2, state, retired } = GetNextMessage(),
        [fill, fill] = get_result(ColSolver2),

        ok = stop(RowSolver1),
        ok = stop(RowSolver2),
        ok = stop(ColSolver1),
        ok = stop(ColSolver2),

        ok

    end(),

    % Feed an ambiguous puzzle:
    % .# | #.
    % #. | .#
    ok = fun() ->

        { ok, RowSolver1 } = start_link(1, 2, [1], self()),
        { RowSolver1, state, priming } = GetNextMessage(),
        { ok, RowSolver2 } = start_link(2, 2, [1], self()),
        { RowSolver2, state, priming } = GetNextMessage(),
        { ok, ColSolver1 } = start_link(1, 2, [1], self()),
        { ColSolver1, state, priming } = GetNextMessage(),
        { ok, ColSolver2 } = start_link(2, 2, [1], self()),
        { ColSolver2, state, priming } = GetNextMessage(),

        ok = prime(RowSolver1, [ColSolver1, ColSolver2]),
        { RowSolver1, state, discovering } = GetNextMessage(),
        ok = prime(RowSolver2, [ColSolver1, ColSolver2]),
        { RowSolver2, state, discovering } = GetNextMessage(),
        ok = prime(ColSolver1, [RowSolver1, RowSolver2]),
        { ColSolver1, state, discovering } = GetNextMessage(),
        ok = prime(ColSolver2, [RowSolver1, RowSolver2]),
        { ColSolver2, state, discovering } = GetNextMessage(),

        % This puzzle leads to a dead end.
        { RowSolver1, state, stalled } = GetNextMessage(),
        { RowSolver2, state, stalled } = GetNextMessage(),
        { ColSolver1, state, stalled } = GetNextMessage(),
        { ColSolver2, state, stalled } = GetNextMessage(),

        % Let's offer a hint to one of the solvers in order it reaches a solution.
        % This should cascade hints to the remaining solvers leading to a possible solution to the puzzle.
        ok = hint(RowSolver1, 1, fill),
        { RowSolver1, state, discovering } = GetNextMessage(),
        { RowSolver1, state, resting } = GetNextMessage(),
        { ColSolver2, state, discovering } = GetNextMessage(),
        { ColSolver2, state, resting } = GetNextMessage(),
        { RowSolver2, state, discovering } = GetNextMessage(),
        { RowSolver2, state, resting } = GetNextMessage(),
        { ColSolver1, state, discovering } = GetNextMessage(),
        { ColSolver1, state, resting } = GetNextMessage(),

        ok = retire(RowSolver1),
        { RowSolver1, state, retired } = GetNextMessage(),
        [fill, gap] = get_result(RowSolver1),
        ok = retire(RowSolver2),
        { RowSolver2, state, retired } = GetNextMessage(),
        [gap, fill] = get_result(RowSolver2),
        ok = retire(ColSolver1),
        { ColSolver1, state, retired } = GetNextMessage(),
        [fill, gap] = get_result(ColSolver1),
        ok = retire(ColSolver2),
        { ColSolver2, state, retired } = GetNextMessage(),
        [gap, fill] = get_result(ColSolver2),

        ok = stop(RowSolver1),
        ok = stop(RowSolver2),
        ok = stop(ColSolver1),
        ok = stop(ColSolver2),

        ok

    end(),

    ok.


% Module API

str_to_map(Str) ->
    lists:filtermap(
        fun(Char) ->
            case Char of
                $# -> {true,fill};
                $. -> {true,gap};
                $? -> {true,unknown};
                $  -> false
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

start(Id, Length, Fills, Listener) ->
    gen_statem:start(
        ?MODULE,
        #state{id=Id, length=Length, fills=Fills, listener=Listener},
        []).

start_link(Id, Length, Fills, Listener) ->
    gen_statem:start_link(
        ?MODULE,
        #state{id=Id, length=Length, fills=Fills, listener=Listener},
        []).

prime(Solver, Solvers) ->
    gen_statem:cast(Solver, { prime, Solvers }).

retire(Solver) ->
    gen_statem:call(Solver, retire).

get_result(Solver) ->
    gen_statem:call(Solver, get_result).

stop(Solver) ->
    gen_statem:stop(Solver).


% Mandatory gen_statem callbacks

callback_mode() -> [state_functions, state_enter].

init(S) ->
    case check_inputs(S#state.fills, S#state.length) of
        false -> { stop, invalid };
        true -> { ok, priming, S }
    end.


% State functions

priming(enter, _, S) ->
    tell(S, priming),
    keep_state_and_data;
priming(cast, { hint, _, _ }, _) ->
    { keep_state_and_data, postpone };
priming(cast, {prime, Solvers}, S) ->
    Clue = emerge_clue(map_combine(S#state.fills, S#state.length)),
    hint_solvers(S#state.id, Solvers, Clue),
    NS = S#state{solvers=Solvers, clue=Clue},
    case lists:member(unknown, Clue) of
        true ->
            { next_state, discovering, NS };
        false ->
            { next_state, resting, NS }
    end;
priming({call, From}, retire, S) ->
    { next_state, retired, S, { reply, From, ok } }.

discovering(enter, _, S) ->
    tell(S, discovering),
    { keep_state_and_data, ?StallTimeout };
discovering(timeout, _, S) ->
    { next_state, stalled, S };
discovering(cast, { hint, Position, Hint }, S) ->
    { HintCategory, HintedClue } = acknowledge_hint(S#state.clue, Position, Hint),
    case HintCategory of
        nonsense ->
            badhint(S),
            keep_state_and_data;
        known ->
            keep_state_and_data;
        useful ->
            NewClue = emerge_clue(HintedClue, map_combine(S#state.fills, S#state.length)),
            hint_solvers(S#state.id, S#state.solvers, emerge_hints(HintedClue, NewClue)),
            NS = S#state{clue=NewClue},
            case lists:member(unknown, NewClue) of
                true ->
                    { keep_state, NS };
                false ->
                    { next_state, resting, NS }
            end
    end;
discovering({call, From}, retire, S) ->
    { next_state, retired, S, { reply, From, ok } }.

stalled(enter, _, S) ->
    tell(S, stalled),
    keep_state_and_data;
stalled(cast, { hint, _, _ }, S) ->
    { next_state, discovering, S, postpone };
stalled({call, From}, retire, S) ->
    { next_state, retired, S, { reply, From, ok } }.

resting(enter, _, S) ->
    tell(S, resting),
    keep_state_and_data;
resting(cast, { hint, Position, Hint }, S) ->
    { HintCategory, _ } = acknowledge_hint(S#state.clue, Position, Hint),
    case HintCategory of
        nonsense -> badhint(S);
        _ -> ok
    end,
    keep_state_and_data;
resting({call, From}, retire, S) ->
    { next_state, retired, S, { reply, From, ok } }.

retired(enter, _, S) ->
    tell(S, retired),
    keep_state_and_data;
retired({call, From}, get_result, S) ->
    { keep_state_and_data, { reply, From, S#state.clue } }.


% Support functions

hint(Solver, Position, Hint) ->
    gen_statem:cast(Solver, { hint, Position, Hint }).

tell(S, Atom) ->
    S#state.listener ! { self(), state, Atom }.

badhint(S) ->
    S#state.listener ! { self(), badhint }.

check_inputs([], _) -> false;
check_inputs(Fills, Max) ->
    is_list(Fills)
    andalso lists:all(fun(Val) -> Val > 0 andalso Val =< Max end, Fills).

% emerge_hints([fill,gap,unknown,unknown],[fill,gap,fill,gap]) -> [unknown,unknown,fill,gap]
emerge_hints(OldHints, NewHints) ->
    lists:foldr(
        fun({Old,New}, Acc) ->
            [case {Old,New} of
                 {unknown,_} -> New;
                 _ -> unknown
             end|Acc]
        end,
        [],
        lists:zip(OldHints, NewHints)
        ).

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
    hint(Solver, Position, Hint),
    hint_solvers(Position, Solvers, Hints).

map_combine(Fills, Length) -> lists:map(fun picr_to_map/1, picr_combine(Fills, Length)).

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
    % io:format("~s~n", [element(2, process_info(self(), backtrace))]),
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
