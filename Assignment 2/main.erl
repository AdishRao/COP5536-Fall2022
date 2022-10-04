-module(main).

-compile(export_all).

-define(DEATHPROB, 1).

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
    start_run(Algo, PIDS),
    T2 = erlang:timestamp(),

    io:fwrite("Total clock time: ~p microseconds(10^-9)\n", [timer:now_diff(T2, T1)]),
    exit(done).

% Start gossip algo

start_run(gossip, PIDS) ->
    Len = length(PIDS),
    N = rand:uniform(Len),
    PID = lists:nth(N, PIDS),
    PID ! {msg, "This is the rumor"},
    wait_complete(gossip, PIDS);

% Start push sum algo
start_run('push-sum', PIDS) ->
    ok.

wait_complete(gossip, []) ->
    ok;
wait_complete(gossip, PIDS) ->
    receive
        {done, PID} ->
            io:fwrite("PID ~p has finished waiting on remaining ~p\n", [PID, PIDS]),
            wait_complete(gossip, lists:delete(PID, PIDS))
    after
        10000 ->
            io:fwrite("Failed after 10s, too many dead nodes\n")
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
    %io:fwrite("I am ~p and I can talk to ~p\n\n", [self(), MyNeighbors]).
    ok.


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
    Death = rand:uniform(),
    if Death =< ?DEATHPROB ->
        dead;
    true ->    
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
        end
    end.