-module(bitcoin_client).
-export([start/1]).

start(Server_node)->
    net_adm:ping(Server_node),
    {server, Server_node} ! {node, node()}.