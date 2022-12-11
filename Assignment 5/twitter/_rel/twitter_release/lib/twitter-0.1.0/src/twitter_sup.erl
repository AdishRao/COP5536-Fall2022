-module(twitter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	io:fwrite("********* IN START LINK *********\n\n\n"),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	io:fwrite("********* IN SUP INIT *********\n\n\n"),
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
