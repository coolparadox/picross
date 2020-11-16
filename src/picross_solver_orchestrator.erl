-module(picross_solver_orchestrator).
-behaviour(gen_event).
-export([test/0]).
-export([map_to_str/1, str_to_map/1, start/3, start_link/3, stop/1, get_map/1]).
-export([init/1, terminate/2, handle_call/2, handle_event/2]).
-record(state, { listener, row_solvers, col_solvers, is_good_result=true, is_stalled=false, solver_states=maps:new() }).

test() ->

    "##\n.#\n" = map_to_str([[fill,fill],[gap,fill]]),
    [[fill,fill],[gap,fill]] = str_to_map("##\n.#\n"),

    GetNextMessage = fun() ->
        receive
            solved -> solved;
            stalled -> stalled;
            nonsense -> nonsense
        end
    end,

    % Unique solution
    ok = fun() ->
        { ok, Orchestrator } = start_link(self(), [[2],[1]], [[1],[2]]),
        solved = GetNextMessage(),
        [[fill,fill],[gap,fill]] = get_map(Orchestrator),
        stop(Orchestrator),
        ok
    end(),

    % Non unique solution
    ok = fun() ->
        { ok, Orchestrator } = start_link(self(), [[1],[1]], [[1],[1]]),
        stalled = GetNextMessage(),
        stop(Orchestrator),
        ok
    end(),

    % Invalid puzzle
    ok = fun() ->
        { ok, Orchestrator } = start_link(self(), [[2],[2]], [[2],[1]]),
        nonsense = GetNextMessage(),
        stop(Orchestrator),
        ok
    end(),

    ok.

% module API

map_to_str(Maps) ->
    lists:foldl(fun(Map, Acc) -> Acc ++ picross_solver:map_to_str(Map) ++ "\n" end, "", Maps).

str_to_map(Str) ->
    lists:map(fun(S) -> picross_solver:str_to_map(S) end, lines(Str)).

start(Listener, RowsFills, ColsFills) ->
    start_orchestrator(Listener, RowsFills, ColsFills, fun gen_event:start/0).

start_link(Listener, RowsFills, ColsFills) ->
    start_orchestrator(Listener, RowsFills, ColsFills, fun gen_event:start_link/0).

stop(Orchestrator) ->
    gen_event:stop(Orchestrator).

get_map(Orchestrator) ->
    gen_event:call(Orchestrator, ?MODULE, get_map).

% gen_event callbacks

init([Listener, RowsFills, ColsFills]) ->
    StartSolvers = fun(Size, Fills) ->
        lists:map(fun({ Id, Fill }) -> { ok, Pid } = picross_solver:start_link(Id, Size, Fill, self()), Pid end, lists:zip(lists:seq(1, length(Fills)), Fills))
    end,
    RowSolvers = StartSolvers(length(ColsFills), RowsFills),
    ColSolvers = StartSolvers(length(RowsFills), ColsFills),
    PrimeSolvers = fun(Solvers, Listeners) ->
        lists:foreach(fun(Solver) -> picross_solver:prime(Solver, Listeners) end, Solvers)
    end,
    PrimeSolvers(RowSolvers, ColSolvers),
    PrimeSolvers(ColSolvers, RowSolvers),
    { ok, #state{listener=Listener, row_solvers=RowSolvers, col_solvers=ColSolvers} }.

terminate(_, S) ->
    lists:foreach(fun picross_solver:stop/1, S#state.row_solvers ++ S#state.col_solvers).

handle_call(get_map, S) ->
    { ok, lists:map(fun picross_solver:get_map/1, S#state.row_solvers), S }.

handle_event({ Solver, state, SolverState }, S) when SolverState == discovering orelse SolverState == priming ->
    { ok, S#state{solver_states=maps:put(Solver, discovering, S#state.solver_states)} };

handle_event({ Solver, state, SolverState }, S) when SolverState == resting orelse SolverState == stalled ->
    NewSolverStates = maps:put(Solver, SolverState, S#state.solver_states),
    case lists:member(discovering, maps:values(NewSolverStates)) of
        true ->
            { ok, S#state{solver_states=NewSolverStates} };
        false ->
            lists:foreach(fun picross_solver:retire/1, maps:keys(NewSolverStates)),
            { ok, S#state{
                is_stalled=lists:member(stalled, maps:values(NewSolverStates)),
                solver_states=NewSolverStates} }
    end;

handle_event({ Solver, state, SolverState }, S) when SolverState == retired ->
    NewSolverStates = maps:put(Solver, SolverState, S#state.solver_states),
    case lists:all(fun(State) -> case State of retired -> true; _ -> false end end, maps:values(NewSolverStates)) of
        true ->
            S#state.listener ! case S#state.is_good_result of
                false -> nonsense;
                true ->
                    case S#state.is_stalled of
                        true -> stalled;
                        false -> solved
                    end
            end;
        false ->
            ok
    end,
    { ok, S#state{solver_states=maps:put(Solver, SolverState, NewSolverStates)} };

handle_event({ _, badhint }, S) ->
    { ok, S#state{is_good_result=false} }.

% support

lines(Str) -> lines(Str, "").

lines([$\n], LineAcc) -> [lists:reverse(LineAcc)];
lines([$\n|Str], LineAcc) -> [lists:reverse(LineAcc)|lines(Str, "")];
lines([Char|Str], LineAcc) -> lines(Str, [Char|LineAcc]).

start_orchestrator(Listener, RowsFills, ColsFills, StartFun) ->
    case check_inputs(RowsFills, ColsFills) of
        false -> badarg;
        true ->
            {ok, Orchestrator} = StartFun(),
            ok = gen_event:add_handler(Orchestrator, ?MODULE, [Listener, RowsFills, ColsFills]),
            {ok, Orchestrator}
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

