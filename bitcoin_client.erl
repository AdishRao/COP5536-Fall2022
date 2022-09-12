-module(bitcoin_client).
-export([start/2]).

start(Server, Args)->
    Server ! {self(), {Args}},
    receive
        {Server, Result}->
            Result
    end.

