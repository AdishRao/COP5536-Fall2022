-module(commands).

-compile(export_all).
start() ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    code:add_pathz("/Users/adish/Desktop/UF/FALL 2022/DOSP/COP5536-Fall2022/Assignment 4/rebar3/_build/default/lib/mysql/ebin").