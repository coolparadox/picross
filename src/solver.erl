-module(solver).
-compile(export_all).
-record(state, {
          tag,
          id,
          length,
          fills,
          clue,
          manager,
          solvers=[],
          stalled=false
         }).

test() ->

    % Solve '1' in the first row of a puzzle with 1 column.
    Solver1 = start_link(1, 1, [1], tag, self()),
    setSolvers(Solver1, lists:map(fun(Id) -> spawn_link(solver, messageForwarder, [Id, self()]) end, [1])),
    solve(Solver1),
    % This puzzle can be solved immediately.
    { Solver1, done, [fill] } = nextMessage(),
    % Also the solver of the first column should be hinted that its first position is filled.
    { 1, { hint, 1, fill } } = nextMessage(),
    % Verify termination.
    terminate(Solver1),
    { Solver1, terminated } = nextMessage(),

    % Same as above, but the solver receives a nonsense hint.
    Solver1a = start_link(1, 1, [1], tag, self()),
    setSolvers(Solver1a, lists:map(fun(Id) -> spawn_link(solver, messageForwarder, [Id, self()]) end, [1])),
    solve(Solver1a),
    { Solver1a, done, [fill] } = nextMessage(),
    { 1, { hint, 1, fill } } = nextMessage(),
    % Simulate the solver of the column hinting an absurd information.
    % The bad hint should be acknowledged.
    Solver1a ! { hint, 1, gap },
    { Solver1a, badhint } = nextMessage(),
    { Solver1a, done, [fill] } = nextMessage(),
    % Verify termination.
    terminate(Solver1a),
    { Solver1a, terminated } = nextMessage(),

    % Solve '2' in the fifth row of a puzzle with 3 columns.
    Solver2 = start_link(5, 3, [2], tag, self()),
    setSolvers(Solver2, lists:map(fun(Id) -> spawn_link(solver, messageForwarder, [Id, self()]) end, [1, 2, 3])),
    solve(Solver2),
    % The initial information is only enough to determine that the middle position is filled, ie: '?#?'
    % The solver of the second column should be hinted that its fifth position is a fill.
    { 2, { hint, 5, fill } } = nextMessage(),
    % No further possible work with the available information; check for stall detection.
    { Solver2, stalled } = nextMessage(),
    % Let's simulate receiving a hint from the solver of the first column.
    Solver2 ! { hint, 1, gap },
    % This is a useful information that leads to the solution '.##'
    % and subsequently hinting the remaining solver of the third column.
    { Solver2, working } = nextMessage(),
    { Solver2, done, [gap, fill, fill] } = nextMessage(),
    { 3, { hint, 5, fill } } = nextMessage(),
    % Verify termination.
    terminate(Solver2),
    { Solver2, terminated } = nextMessage(),

    ok.

messageForwarder(Tag, Pid) ->
    receive
        Message ->
            Pid ! { Tag, Message },
            messageForwarder(Tag, Pid)
    after 1000 -> ok
    end.

nextMessage() ->
    receive
        Message -> Message
    after 200 ->
              error(timeout)
    end.

start(Id, Length, Fills, Tag, Manager) ->
    spawn(?MODULE, init, [Id, Length, Fills, Tag, Manager]).

start_link(Id, Length, Fills, Tag, Manager) ->
    spawn_link(?MODULE, init, [Id, Length, Fills, Tag, Manager]).

init(Id, Length, Fills, Tag, Manager) ->
    case check_inputs(Fills, Length) of
        false -> badarg;
        true ->
            solver(working, #state{tag=Tag, id=Id, length=Length, fills=Fills, clue=emergeClue(mapCombine(Fills, Length)), manager=Manager})
    end.

setSolvers(Solver, Solvers) ->
    Solver ! { solvers, Solvers }.

solve(Solver) ->
    Solver ! solve.

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
        solve ->
            hintSolvers(S#state.id, S#state.solvers, S#state.clue),
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
            { HintQuality, HintedClue } = acknowledgeHint(S#state.clue, Position, Hint),
            case HintQuality of
                nonsense ->
                    S#state.manager ! { self(), badhint },
                    solver(working, S#state{stalled=false});
                known ->
                    solver(working, S#state{stalled=false});
                useful ->
                    NewClue = emergeClue(HintedClue, mapCombine(S#state.fills, S#state.length)),
                    hintSolvers(S#state.id, S#state.solvers, emergeHints(HintedClue, NewClue)),
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
    after case S#state.stalled of true -> infinity; false -> 100 end ->
        S#state.manager ! { self(), stalled },
        solver(working, S#state{stalled=true})
    end;
solver(done, S) ->
    S#state.manager ! { self(), done, S#state.clue },
    receive
        { hint, Position, Hint } ->
            { HintQuality, _ } = acknowledgeHint(S#state.clue, Position, Hint),
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
