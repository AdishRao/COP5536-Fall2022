-module(main).

-compile(export_all).

start_server(NoNodes, Topology, Algo) ->
    register(server, self()),
    spawn_actors(NoNodes, Topology, Algo).

% Spawn required number of nodes and provide the algorithm being used (gossip or push-sum)
spawn_many(0, _) ->
    [];
spawn_many(N, Algo) ->
        PID = spawn(main, start_gossip, [Algo]),
        [PID] ++ spawn_many(N - 1, Algo).

% Spawn required number of actors for line topology and build line topology
spawn_actors(NoNodes, line, Algo) ->
    PIDS = spawn_many(NoNodes, Algo),
    send_neighbors(line, PIDS, max, length(PIDS)).


send_neighbors(line, _, max, 1) ->
    exit('not enough nodes, at least 2');
send_neighbors(line, PIDS, max, N) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS)]},
    send_neighbors(line, PIDS, N-1).
send_neighbors(line, _, 0) ->
    ok;
send_neighbors(line, PIDS, 1) ->
    lists:nth(1, PIDS) ! {neighbor, [lists:nth(2 , PIDS)]},
    send_neighbors(line, PIDS, 0);
send_neighbors(line, PIDS, N) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS)]},
    send_neighbors(line, PIDS, N-1).

% Actor waits for its neighbors
start_gossip(Algo) ->
    receive
        {neighbor, MyNeighbors} -> 
            gossip(MyNeighbors, Algo)
    end.

gossip(MyNeighbors, gossip) ->
    io:fwrite("I am ~p and I can talk to ~p\n\n", [self(), MyNeighbors]);

gossip(MyNeighbors, pushsum) ->
    io:fwrite("I am ~p and I can talk to ~p\n\n", [self(), MyNeighbors]).