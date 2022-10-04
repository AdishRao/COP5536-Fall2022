-module(main).

-compile(export_all).

-define(DEATHPROB, 0.1).

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
    Status = start_run(Algo, PIDS),
    T2 = erlang:timestamp(),

    io:fwrite("Total clock time: ~p microseconds(10^-9)\n", [timer:now_diff(T2, T1)]),
    io:fwrite("~p, ~p, ~p, ~p, ~p, ~p\n", [Algo, Topology, timer:now_diff(T2, T1), length(PIDS), ?DEATHPROB, Status]),
    exit(done).

% Start gossip algo

start_run(gossip, PIDS) ->
    Len = length(PIDS),
    N = rand:uniform(Len),
    PID = lists:nth(N, PIDS),
    PID ! {msg, "This is the rumor"},
    Status = wait_complete(gossip, PIDS, ok),
    Status;

% Start push sum algo
start_run('push-sum', PIDS) ->
    Len = length(PIDS),
    N = rand:uniform(Len),
    PID = lists:nth(N, PIDS),
    PID ! {start},
    Status = wait_complete('push-sum', PIDS, ok),
    Status.

wait_complete(gossip, [], Status) ->
    Status;
wait_complete(gossip, PIDS, Status) ->
    receive
        {done, PID} ->
            %io:fwrite("PID ~p has finished waiting on remaining ~p\n", [PID, PIDS]),
            io:fwrite("PID ~p has finished\n", [PID]),
            wait_complete(gossip, lists:delete(PID, PIDS), Status);
        {dead, PID} ->
            %io:fwrite("PID ~p has finished waiting on remaining ~p\n", [PID, PIDS]),
            io:fwrite("PID ~p has died\n", [PID]),
            wait_complete(gossip, lists:delete(PID, PIDS), 'dead_node_success')
    after
        10000 ->
            io:fwrite("Failed after 10s, too many dead nodes\n"),
            failed
    end;

wait_complete('push-sum', [], Status) ->
    Status;
wait_complete('push-sum', PIDS, Status) ->
    receive
        {done, PID} ->
            %io:fwrite("PID ~p has finished waiting on remaining ~p\n", [PID, PIDS]),
            io:fwrite("PID ~p has finished\n", [PID]),
            wait_complete(gossip, lists:delete(PID, PIDS), Status);
        {dead, PID} ->
            %io:fwrite("PID ~p has finished waiting on remaining ~p\n", [PID, PIDS]),
            io:fwrite("PID ~p has died\n", [PID]),
            wait_complete(gossip, lists:delete(PID, PIDS), 'dead_node_success')
    after
        100000 ->
            io:fwrite("Failed after 100s, too many dead nodes\n"),
            failed

    end.

% Spawn required number of nodes and provide the algorithm being used (gossip or push-sum).
spawn_many(0, _) ->
    [];
spawn_many(N, Algo) ->
        PID = spawn_link(main, start_gossip, [Algo]),
        [PID] ++ spawn_many(N - 1, Algo).

%---------------------------------------------SPAWN ACTORS--------------------------------------------
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
    TotalNoNodesRoot = trunc(math:ceil(math:sqrt(NoNodes))),
    TotalNoNodes = TotalNoNodesRoot*TotalNoNodesRoot,
    if TotalNoNodes < 4 ->
        exit('Not enough nodes');
    true ->
        PIDS = spawn_many(TotalNoNodes, Algo),
        send_neighbors(grid_2d, PIDS, TotalNoNodesRoot, TotalNoNodes),
        PIDS
    end;

% Spawn required number of actors for line topology and build 2d grid topology.
spawn_actors(NoNodes, grid_3d, Algo) ->
    EdgeLength = trunc(math:ceil(math:pow(NoNodes, 1/3))),
    TotalNoNodes = trunc(math:pow(EdgeLength, 3)),
    if TotalNoNodes < 8 ->
        exit('Not enough nodes');
    true ->
        PIDS = spawn_many(TotalNoNodes, Algo),
        send_neighbors(grid_3d, PIDS, EdgeLength, TotalNoNodes),
        PIDS
    end.
%----------------------Sending Neighbors-----------------------------------------------------------------------

%left edge
send_node_neighbors(1, Side, N, PIDS) -> %4
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), lists:nth(N + Side, PIDS), lists:nth(N - Side, PIDS)]}; 

%right edge
send_node_neighbors(0, Side, N, PIDS) -> %4
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + Side, PIDS), lists:nth(N - Side, PIDS)]}; 

% Send node neighbors for nodes not on the edge of the 2d plane
send_node_neighbors(_, Side, N, PIDS) -> %4
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + Side, PIDS), lists:nth(N + 1, PIDS), lists:nth(N - Side, PIDS)]}. 

% Bottom row
send_node_neighbors(edge, true, Side, N, 1, PIDS) -> %6
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), lists:nth(N - Side, PIDS)]}; 

send_node_neighbors(edge, true, Side, N, 0, PIDS) -> %6
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N - Side, PIDS)]}; 

send_node_neighbors(edge, true, Side, N, _, PIDS) -> %6
    %inner  nodes
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS),lists:nth(N - Side, PIDS)]}; 

%Top row
send_node_neighbors(edge, false, Side, N, 1, PIDS) -> %6
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), lists:nth(N + Side, PIDS)]}; 

send_node_neighbors(edge, false, Side, N, 0, PIDS) -> %6
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + Side, PIDS)]}; 

send_node_neighbors(edge, false, Side, N, _, PIDS) -> %6
    % inner node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS),lists:nth(N + Side, PIDS)]}.

%Three dim - inner planes ----------------------------
%left edge
send_node_neighbors(1, Side, N, PIDS, threeDim) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)
        ]}; 

%right edge
send_node_neighbors(0, Side, N, PIDS, threeDim) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

% Send node neighbors for nodes not on the edge of the 2d plane
send_node_neighbors(_, Side, N, PIDS, threeDim) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N + 1, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]};

%Three dim - first plane ----------------------------
%left edge
send_node_neighbors(1, Side, N, PIDS, firstPlane) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)
        ]}; 

%right edge
send_node_neighbors(0, Side, N, PIDS, firstPlane) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

% Send node neighbors for nodes not on the edge of the 2d plane
send_node_neighbors(_, Side, N, PIDS, firstPlane) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N + 1, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]};

%Three dim - last plane ----------------------------
%left edge
send_node_neighbors(1, Side, N, PIDS, lastPlane) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)
        ]}; 

%right edge
send_node_neighbors(0, Side, N, PIDS, lastPlane) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

% Send node neighbors for nodes not on the edge of the 2d plane
send_node_neighbors(_, Side, N, PIDS, lastPlane) -> %5
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS), 
        lists:nth(N + 1, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS) ]}.


% Bottom row
send_node_neighbors(edge, true, Side, N, 1, PIDS, threeDim) -> %7
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS),
        lists:nth(N - Side, PIDS), 
        % lists:nth(N + Side, PIDS), 
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, true, Side, N, 0, PIDS, threeDim) -> %7
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, true, Side, N, _, PIDS, threeDim) -> %7
    %inner  nodes
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + 1, PIDS),
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 
%Top row
send_node_neighbors(edge, false, Side, N, 1, PIDS, threeDim) -> %7
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), 
        lists:nth(N + Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, false, Side, N, 0, PIDS, threeDim) -> %7
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, false, Side, N, _, PIDS, threeDim) -> %7
    % inner node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + 1, PIDS),
        lists:nth(N + Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]};

%Three dim - first plane ----------------------------


% Bottom row
send_node_neighbors(edge, true, Side, N, 1, PIDS, firstPlane) -> %7
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS),
        lists:nth(N - Side, PIDS), 
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)
    ]}; 

send_node_neighbors(edge, true, Side, N, 0, PIDS, firstPlane) -> %7
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, true, Side, N, _, PIDS, firstPlane) -> %7
    %inner  nodes
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + 1, PIDS),
        lists:nth(N - Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 
%Top row
send_node_neighbors(edge, false, Side, N, 1, PIDS, firstPlane) -> %7
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), 
        lists:nth(N + Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, false, Side, N, 0, PIDS, firstPlane) -> %7
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, false, Side, N, _, PIDS, firstPlane) -> %7
    % inner node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + 1, PIDS),
        lists:nth(N + Side, PIDS),
        lists:nth(N + trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]};

%Three dim - last plane ----------------------------


% Bottom row
send_node_neighbors(edge, true, Side, N, 1, PIDS, lastPlane) -> %7
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS),
        lists:nth(N - Side, PIDS), 
        % lists:nth(N + Side, PIDS), 
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)
    ]}; 

send_node_neighbors(edge, true, Side, N, 0, PIDS, lastPlane) -> %7
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, true, Side, N, _, PIDS, lastPlane) -> %7
    %inner  nodes
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + 1, PIDS),
        lists:nth(N - Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 
%Top row
send_node_neighbors(edge, false, Side, N, 1, PIDS, lastPlane) -> %7
    %left most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N + 1, PIDS), 
        lists:nth(N + Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, false, Side, N, 0, PIDS, lastPlane) -> %7
    %right most node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS),
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}; 

send_node_neighbors(edge, false, Side, N, _, PIDS, lastPlane) -> %7
    % inner node
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), 
        lists:nth(N + 1, PIDS),
        lists:nth(N + Side, PIDS),
        lists:nth(N - trunc(math:pow(Side, 2)), PIDS) ,
        lists:nth(rand:uniform(trunc(math:pow(Side, 3))), PIDS)]}.


% First plane i.e., front most plane
get_neighbors(grid_3d, PIDS, 1, Side, Node_no) -> %5
    if Node_no =< Side ->
         send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS, firstPlane );
    (Node_no + Side > (Side * Side)) ->
        send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS, firstPlane);
    true ->
        send_node_neighbors(Node_no rem Side, Side, Node_no, PIDS, firstPlane)
    end;

% Last plane 
get_neighbors(grid_3d, PIDS, last, Side, Node_no) -> %5
    if Node_no =< Side ->
         send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS, lastPlane);
    (Node_no + Side > (Side * Side)) ->
        send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS, lastPlane);
    true ->
        send_node_neighbors(Node_no rem Side, Side, Node_no, PIDS, lastPlane)
    end;


% inner planes
get_neighbors(grid_3d, PIDS, _, Side, Node_no) -> %5
    if Node_no =< Side ->
         send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS, threeDim );
    (Node_no + Side > (Side * Side)) ->
        send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS, threeDim);
    true ->
        send_node_neighbors(Node_no rem Side, Side, Node_no, PIDS, threeDim)
    end.

% Finished sending neighbors full and line.
send_neighbors(_, _, 0) ->  %3
    ok;
% Find neighbors for full for node N.
send_neighbors(full, PIDS, N) -> %3
    Self = lists:nth(N, PIDS),
    Self ! {neighbor, lists:delete(Self, PIDS)},
    send_neighbors(full, PIDS, N-1);

% Find neighbors for line for node N.
send_neighbors(line, PIDS, 1) -> %3
    lists:nth(1, PIDS) ! {neighbor, [lists:nth(2 , PIDS)]},
    send_neighbors(line, PIDS, 0);

send_neighbors(line, PIDS, N) -> %3
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS), lists:nth(N + 1, PIDS)]},
    send_neighbors(line, PIDS, N-1).  

%Finished sending neighbors in the plane
send_neighbors(grid_2d, _, _, 0)  ->  %4
    ok;

send_neighbors(grid_3d, _, _, 0)  -> %4
    ok;

%Find neighbors for 3D grid
send_neighbors(grid_3d, PIDS, EdgeLength, Node_no) -> %4
    PlaneNumber = Node_no div (EdgeLength*EdgeLength),
    Remainder = Node_no rem (EdgeLength*EdgeLength),
    if Remainder /= 0 ->
        NewPlaneNumber = PlaneNumber + 1;
    true ->
        NewPlaneNumber = PlaneNumber
    end,
    if NewPlaneNumber == EdgeLength ->
        get_neighbors(grid_3d, PIDS, last, EdgeLength, Node_no);
    true ->
        get_neighbors(grid_3d, PIDS, NewPlaneNumber, EdgeLength, Node_no)
    end,
    send_neighbors(grid_3d, PIDS, EdgeLength, Node_no-1);

% Find neighbors for 2D grid
send_neighbors(grid_2d, PIDS, Side, Node_no) -> %4
    if Node_no =< Side ->
         send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS);
    (Node_no + Side > (Side * Side)) ->
        send_node_neighbors(edge, Node_no + Side > (Side * Side), Side, Node_no, Node_no rem Side, PIDS);
    true ->
        send_node_neighbors(Node_no rem Side, Side, Node_no, PIDS)
    end,
    send_neighbors(grid_2d, PIDS, Side, Node_no-1);

% Find neighbors for line, given the last element. Will check that number of nodes is satisfied.
send_neighbors(line, _, max, 1) -> %4
    exit('not enough nodes, at least 2');

send_neighbors(line, PIDS, max, N) -> %4
    lists:nth(N, PIDS) ! {neighbor, [lists:nth(N - 1, PIDS)]},
    send_neighbors(line, PIDS, N-1);

% Find neighbors for full, given the last element. Will check that number of nodes is satisfied.
send_neighbors(full, _, max, 1) -> %4
    exit('not enough nodes, at least 2');

send_neighbors(full, PIDS, max, N) -> %4
    Self = lists:nth(N, PIDS),
    Self ! {neighbor, lists:delete(Self, PIDS)},
    send_neighbors(full, PIDS, N-1).

%--------------------------------GOSSIP and PUSH SUM------------------------------------------------------------------

pid_tokens(Pid) ->
    PidStr = pid_to_list(Pid),
    PidStr1 = lists:sublist(PidStr, 2, length(PidStr)-2),
    [_, P1, _] = [list_to_integer(T) || T <- string:tokens(PidStr1,[$.])],
    P1.

% Actor waits for its neighbors.
start_gossip(Algo) ->
    receive
        {neighbor, MyNeighbors} -> 
            gossip(MyNeighbors, Algo)
    end.


% Run gossip algo.
gossip(MyNeighbors, gossip) ->
    receive
        {msg, Msg} ->
            Death = rand:uniform(),
            if Death =< ?DEATHPROB ->
                server ! {dead, self()};
            true ->
                Len = length(MyNeighbors),
                PID_Index = rand:uniform(Len),
                PID = lists:nth(PID_Index, MyNeighbors),
                PID ! {msg, Msg},
                gossip(MyNeighbors, gossip, Msg, 9)
            end
    end;

% Run pushsum algo.
gossip(MyNeighbors, 'push-sum') ->
    S = pid_tokens(self()),
    W = 1,
    Len = length(MyNeighbors),
    PID_Index = rand:uniform(Len),
    PID = lists:nth(PID_Index, MyNeighbors),
    No_trials = 0,
    receive
        {Rec_S, Rec_W}->
            Death = rand:uniform(),
            if Death =< ?DEATHPROB ->
                server ! {dead, self()};
            true ->
                Send_S = (Rec_S + S) / 2,
                Send_W = (Rec_W + W) / 2,
                PID ! {Send_S, Send_W},
                Cur_S = Send_S,
                Cur_W = Send_W,
                Sum_estimate = Cur_S / Cur_W,
                gossip(MyNeighbors, 'push-sum', Sum_estimate, Cur_S, Cur_W, No_trials)
            end;
        {start} ->
            Death = rand:uniform(),
            if Death =< ?DEATHPROB ->
                server ! {dead, self()};
            true ->
                Send_S = (S) / 2,
                Send_W = (W) / 2,
                PID ! {Send_S, Send_W},
                Cur_S = Send_S,
                Cur_W = Send_W,
                Sum_estimate = Cur_S / Cur_W,
                gossip(MyNeighbors, 'push-sum', Sum_estimate, Cur_S, Cur_W, No_trials)
            end
    end.


% Received 10 times, tell server done.
gossip(MyNeighbors, gossip, StoredMsg, 0) ->
    server ! {done, self()},
    gossip(MyNeighbors, gossip, StoredMsg, -1);
    
% Send message to remaining nodes not yet done.
gossip(MyNeighbors, gossip, StoredMsg, -1) ->
    Len = length(MyNeighbors),
    PID_Index = rand:uniform(Len),
    PID = lists:nth(PID_Index, MyNeighbors),
    PID ! {msg, StoredMsg},
    timer:sleep(100),
    gossip(MyNeighbors, gossip, StoredMsg, -1);

% Loop sending and recieving messages.
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

% Received same value of sum estimate for three consecutive trials, tell server done.
gossip(MyNeighbors, 'push-sum', Sum_estimate, S, W, 3) ->
    server ! {done, self()},
    gossip(MyNeighbors, 'push-sum', Sum_estimate, S, W, -1);

gossip(MyNeighbors, 'push-sum', _, S, W, -1)->
    Len = length(MyNeighbors),
    PID_Index = rand:uniform(Len),
    PID = lists:nth(PID_Index, MyNeighbors),
    receive
        {Rec_S, Rec_W}->
            Send_S = (Rec_S + S) / 2,
            Send_W = (Rec_W + W) / 2
    after
        100 ->
            Send_S = (S) / 2,
            Send_W = (W) / 2
    end,
    PID ! {Send_S, Send_W},
    Cur_S = Send_S,
    Cur_W = Send_W,
    New_sum_estimate = Cur_S / Cur_W,
    gossip(MyNeighbors, 'push-sum', New_sum_estimate, Cur_S, Cur_W, -1);

gossip(MyNeighbors, 'push-sum', Sum_estimate, S, W, Same_count)->
    Len = length(MyNeighbors),
    PID_Index = rand:uniform(Len),
    PID = lists:nth(PID_Index, MyNeighbors),
    receive
        {Rec_S, Rec_W}->
            Send_S = (Rec_S + S) / 2,
            Send_W = (Rec_W + W) / 2,
            PID ! {Send_S, Send_W},
            Cur_S = Send_S,
            Cur_W = Send_W,
            New_sum_estimate = Cur_S / Cur_W,
            Diff = New_sum_estimate - Sum_estimate,
            Absdiff = abs(Diff),
            Delta = math:pow(10, -10),
            if (Absdiff < Delta) ->
                New_same_count = Same_count + 1;
            true ->
                New_same_count = 0
            end,
            gossip(MyNeighbors, 'push-sum', New_sum_estimate, Cur_S, Cur_W, New_same_count)
    after
        100 ->
            Send_S = (S) / 2,
            Send_W = (W) / 2,
            PID ! {Send_S, Send_W},
            Cur_S = Send_S,
            Cur_W = Send_W,
            %io:fwrite("Current S ~p and Cur W ~p\n",[Cur_S, Cur_W]),
            New_sum_estimate = Cur_S / Cur_W,
            gossip(MyNeighbors, 'push-sum', New_sum_estimate, Cur_S, Cur_W, Same_count)
    end.