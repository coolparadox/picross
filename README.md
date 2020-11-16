[![Build Status](https://travis-ci.com/coolparadox/picross-erl.svg?branch=master)](https://travis-ci.com/coolparadox/picross-erl)

# picross-erl

*Experimentation on the actor model for solving picross / nonogram puzzles*

[Picross (nonogram)](http://picross.net/how-to-play.php) puzzles are nice time killers with plenty of samples freely available for fun. The rules are simple and the objetive is to discover the hidden image encoded by the enclosing numbers around the puzzle. Check this [simple example](https://en.wikipedia.org/wiki/Nonogram#Example) from Wikipedia.

Solution to these puzzles can be achieved iteractively by *mutual cooperation* between the set of clues for rows and columns: for instance, when a new position of the puzzle is discovered after reasoing over the given clue for a row, it may unlock new information for the crossing column, that in turn helps other rows to proceed, and so on.

This project is a hobbyist stake at appling the [actor model](https://www.brianstorti.com/the-actor-model/) to mimic this approach and try to solve some puzzles by a computer.

## The acting

Each row of the puzzle is an actor whose objective is to solve all of its positions (ie, discover if they're filled or empty -- or black or white if you prefer). Same goes for each column of the puzzle -- so the total number of actors is the sum of rows and columns.

On starting, the actors (called **solvers** from now on) take the numeric clues associated to each row (or column) and try to solve their positions. The amount of solved positions for each actor can be anything from zero to all positions. When a row solver solves a position, it notifies the crossing column solver about the finding, and vice-versa. When a solver receives a consistent hint from another solver, it repeats the cycle of discovering of new positions.

## Solver state machine

As you may have guessed, the aforementioned process of solving a picross puzzle is highly asynchronous and can lead to an increasing storm of messages between the solvers if some edge cases are not properly countered.

A solver can be in one of the following states:

- *PRIMING*: Awaiting to be informed who are the crossing solvers that are interested in new discoveries. In this state, the processing of eventual hints received from other solvers is postponed.
- *DISCOVERING*: Not yet fully solved; awaiting hints from other solvers in the hope this helps advance with the solution.
- *STALLED*: Not yet fully solved, but too much time has passed without hints. If all solvers reach this state, the puzzle has no unique solution.
- *RESTING*: The solver is fully solved, but still checking if eventual hints from other solvers are consistent. Any inconsistent hint (e.g. a filled position is hinted to be empty) is a confirmation that the puzzle has no solution.
- *RETIRED*: Stop all solving. The solution of the puzzle, if any, can be safely retrieved when all solvers reach this state.

The solver state diagram:

![picross-solver state diagram](/doc/solver_states/picross_solver_states.png "picross-solver state diagram")

## Executing

This is not yet deployed as a final aplication. The Erlang shell can be used to access the API:

    $ cd src
    $ erl
    Erlang/OTP 23 [erts-11.1.1] [source] [64-bit] [smp:12:12] [ds:12:12:10] [async-threads:1] [hipe]
    
    Eshell V11.1.1  (abort with ^G)
    1>

Compile modules:

    1> {ok,_} = c(picross_solver_orchestrator), {ok,_} = c(picross_solver), {ok,_} = c(picross).
    {ok,picross}

Run tests:

    2> picross:test().
    ok

Solve a puzzle:

    3> Rows = [[4],[6],[2,2],[2,2],[6],[4],[2],[2],[2]].
    [[4],[6],[2,2],[2,2],[6],[4],[2],[2],[2]]
    4> Cols = [[9],[9],[2,2],[2,2],[4],[4]].
    ["\t","\t",[2,2],[2,2],[4],[4]]
    5> {ok,Solution} = picross:solve(Rows,Cols).
    {ok,[[fill,fill,fill,fill,gap,gap],
         [fill,fill,fill,fill,fill,fill],
         [fill,fill,gap,gap,fill,fill],
         [fill,fill,gap,gap,fill,fill],
         [fill,fill,fill,fill,fill,fill],
         [fill,fill,fill,fill,gap,gap],
         [fill,fill,gap,gap,gap,gap],
         [fill,fill,gap,gap,gap,gap],
         [fill,fill,gap,gap,gap,gap]]}

In a more eye pleasant representation:

    6> io:format(picross:map_to_str(Solution)).
    ####..
    ######
    ##..##
    ##..##
    ######
    ####..
    ##....
    ##....
    ##....
    ok 

## Known issues

Each solver employs a brute-force approach for solving its positions with the initial numerical clue and the set of received hints as inputs. This implementation is highly memory intensive and does not scale to larger puzzles.
