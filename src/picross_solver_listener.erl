-module(picross_solver_listener).
-behaviour(gen_event).
-export([test/0]).
-export([init/1, handle_call/2, handle_event/2]).
-record(state, { listener, is_good_result=true, is_stalled=false, solver_states=maps:new() }).

test() ->

    % FIXME: add tests

    { ok, Manager } = gen_event:start_link(),
    ok = gen_event:add_handler(Manager, picross_solver_listener, self()),

    gen_event:stop(Manager),

    ok.

init(Listener) ->
    { ok, #state{listener=Listener} }.

handle_call(_, _) ->
    exit("unexpected call").

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
    case lists:all(fun(State) -> case State of retired -> true; _ -> false end end, maps:values(S#state.solver_states)) of
        true ->
            S#state.listener ! case S#state.is_good_result of
                false -> nonsense;
                true ->
                    case S#state.is_stalled of
                        true -> stalled;
                        false -> ok
                    end
            end;
        false ->
            ok
    end,
    { ok, S#state{solver_states=maps:put(Solver, SolverState, S#state.solver_states)} };

handle_event({ _, badhint }, S) ->
    { ok, S#state{is_good_result=false} }.
