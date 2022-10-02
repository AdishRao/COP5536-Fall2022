-module(main).

-compile(export_all).

start(NoNodes, Topology, Algo) ->
    spawn_link(main, start_server, [NoNodes, Topology, Algo]),
    process_flag(trap_exit, true),
    receive
        {'EXIT', _, done} ->
            ok;
        {'EXIT', _, Reason} ->
            exit(Reason)
    end.
        

start_server(NoNodes, Topology, Algo) ->
    register(server, self()),
    PIDS = spawn_actors(NoNodes, Topology, Algo),
    T1 = erlang:timestamp(),
    %{Time, _} = timer:tc(main, start_run, [Algo, PIDS]),
    start_run(Algo, PIDS),
    T2 = erlang:timestamp(),

    io:fwrite("Total clock time: ~p microseconds(10^-9)\n", [timer:now_diff(T2, T1)]),
    exit(done).

start_run(gossip, PIDS) ->
    Len = length(PIDS),
    N = rand:uniform(Len),
    PID = lists:nth(N, PIDS),
    PID ! {msg, "This is the rumor"},
    wait_complete(gossip, PIDS).

wait_complete(gossip, []) ->
    ok;
wait_complete(gossip, PIDS) ->
    receive
        {done, PID} ->
            io:fwrite("PID ~p has finished waiting on remaining ~p\n", [PID, PIDS]),
            wait_complete(gossip, lists:delete(PID, PIDS))
    end.

% Spawn required number of nodes and provide the algorithm being used (gossip or push-sum).
spawn_many(0, _) ->
    [];
spawn_many(N, Algo) ->
        PID = spawn_link(main, start_gossip, [Algo]),
        [PID] ++ spawn_many(N - 1, Algo).

% Spawn required number of actors for line topology and build line topology.
spawn_actors(NoNodes, line, Algo) ->
    PIDS = spawn_many(NoNodes, Algo),
    send_neighbors(line, PIDS, max, length(PIDS)),
    PIDS;

% Spawn required number of actors for line topology and build line topology.
spawn_actors(NoNodes, full, Algo) ->
    PIDS = spawn_many(NoNodes, Algo),
    send_neighbors(full, PIDS, max, length(PIDS)),
    PIDS;

% Spawn required number of actors for line topology and build 2d grid topology.
spawn_actors(NoNodes, grid_2d, Algo) ->
    % Side = floor(math:sqrt(NoNodes)),
    PIDS = spawn_many(NoNodes, Algo),
    send_neighbors(grid_2d, PIDS, trunc(math:sqrt(NoNodes)), NoNodes),
    PIDS.

%left edge
send_node_neighbors(1, Side, N, PIDS) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), lists:nth(N + Side, PIDS), lists:nth(N - Side, PIDS)]};

%right edge
send_node_neighbors(0, Side, N, PIDS) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + Side, PIDS), lists:nth(N - Side, PIDS)]};

% Send node neighbors for nodes not on the edge of the 2d plane
send_node_neighbors(_, Side, N, PIDS) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + Side, PIDS), lists:nth(N + 1, PIDS), lists:nth(N - Side, PIDS) ]}.

% Bottom row
send_node_neighbors(edge, true, Side, N, 1, PIDS) ->
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), lists:nth(N - Side, PIDS)]};

send_node_neighbors(edge, true, Side, N, 0, PIDS) ->
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N - Side, PIDS)]};

send_node_neighbors(edge, true, Side, N, _, PIDS) ->
    %inner  nodes
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS),lists:nth(N - Side, PIDS)]};

%Top row
send_node_neighbors(edge, false, Side, N, 1, PIDS) ->
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), lists:nth(N + Side, PIDS)]};

send_node_neighbors(edge, false, Side, N, 0, PIDS) ->
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + Side, PIDS)]};

send_node_neighbors(edge, false, Side, N, _, PIDS) ->
    % inner node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS),lists:nth(N + Side, PIDS)]}.

% % Finished all nodes
% send_node_neighbors(edge, _, Side, 0, _, PIDS) ->
%     ok;

%Finished sending neighbors in the plane
send_neighbors(grid_2d, _, _, 0)  ->
    ok;

% Find neighbors for 2D grid
send_neighbors(grid_2d, PIDS, Side, Node_no) ->
    io:fwrite("Inside send_neighbors, Running for node - ~p \n\n", [Node_no]),
    if Node_no =< Side or (Node_no + Side > (Side * Side)) ->
        % send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, math:mod(Node_no, Side), PIDS);
         send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS);
    true ->
        % send_node_neighbors(math:mod(Node_no, Side), Side, Node_no, PIDS)
        send_node_neighbors(Node_no rem Side, Side, Node_no, PIDS)
    end,
    send_neighbors(grid_2d, PIDS, Side, Node_no-1);


% Find neighbors for line, given the last element. Will check that number of nodes is satisfied.
send_neighbors(line, _, max, 1) ->
    exit('not enough nodes, at least 2');
send_neighbors(line, PIDS, max, N) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS)]},
    send_neighbors(line, PIDS, N-1);

% Find neighbors for full, given the last element. Will check that number of nodes is satisfied.
send_neighbors(full, _, max, 1) ->
    exit('not enough nodes, at least 2');
send_neighbors(full, PIDS, max, N) ->
    Self = lists:nth(N, PIDS),
    Self ! {neighbor, lists:delete(Self, PIDS)},
    send_neighbors(full, PIDS, N-1).



% Finished sending neighbors.
send_neighbors(_, _, 0) ->
    ok;

% Find neighbors for full for node N.
send_neighbors(full, PIDS, N) ->
    Self = lists:nth(N, PIDS),
    Self ! {neighbor, lists:delete(Self, PIDS)},
    send_neighbors(full, PIDS, N-1);

% Find neighbors for line for node N.
send_neighbors(line, PIDS, 1) ->
    lists:nth(1, PIDS) ! {neighbor, [lists:nth(2 , PIDS)]},
    send_neighbors(line, PIDS, 0);
send_neighbors(line, PIDS, N) ->
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS)]},
    send_neighbors(line, PIDS, N-1).

% Actor waits for its neighbors.
start_gossip(Algo) ->
    receive
        {neighbor, MyNeighbors} -> 
            gossip(MyNeighbors, Algo)
    end.

% Run gossip algo.
gossip(MyNeighbors, gossip) ->
    %io:fwrite("I am ~p and I can talk to ~p\n\n", [self(), MyNeighbors]);
    receive
        {msg, Msg} ->
            Len = length(MyNeighbors),
            PID_Index = rand:uniform(Len),
            PID = lists:nth(PID_Index, MyNeighbors),
            PID ! {msg, Msg},
            gossip(MyNeighbors, gossip, Msg, 9)
    end;
% Run pushsum algo.
gossip(MyNeighbors, 'push-sum') ->
    io:fwrite("I am ~p and I can talk to ~p\n\n", [self(), MyNeighbors]).


gossip(MyNeighbors, gossip, StoredMsg, 0) ->
    server ! {done, self()},
    gossip(MyNeighbors, gossip, StoredMsg, -1);

gossip(MyNeighbors, gossip, StoredMsg, -1) ->
    Len = length(MyNeighbors),
    PID_Index = rand:uniform(Len),
    PID = lists:nth(PID_Index, MyNeighbors),
    PID ! {msg, StoredMsg},
    timer:sleep(100),
    gossip(MyNeighbors, gossip, StoredMsg, -1);

gossip(MyNeighbors, gossip, StoredMsg, N) ->
    receive
        {msg, Msg} ->
            Len = length(MyNeighbors),
            PID_Index = rand:uniform(Len),
            PID = lists:nth(PID_Index, MyNeighbors),
            PID ! {msg, Msg},
            gossip(MyNeighbors, gossip, Msg, N-1)
    after
        0 ->
            Len = length(MyNeighbors),
            PID_Index = rand:uniform(Len),
            PID = lists:nth(PID_Index, MyNeighbors),
            PID ! {msg, StoredMsg},
            gossip(MyNeighbors, gossip, StoredMsg, N)
    end.