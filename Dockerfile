FROM debian:latest
RUN apt-get --yes --no-upgrade update
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get --yes --no-upgrade --no-install-recommends install erlang-base
ADD src /work
WORKDIR /work
RUN erlc *.erl
RUN erl -noshell -run picross_solver test -run init stop
RUN erl -noshell -run picross_solver_orchestrator test -run init stop
RUN erl -noshell -run picross test -run init stop
