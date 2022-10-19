-module(main).

-compile(export_all).

-define(M, 160).

start(NumNodes, NumRequests) ->
    PIDS = spawn_many(numNodes, max),
    build_chord(lists:sublist(PIDS, 2, NumNodes), [lists:nth(1, PIDS)]).

build_chord([], _) ->
    ok;
build_chord(Remaining, Joined) ->
    KnownNodePID = lists:nth(rand:uniform(length(Joined)), Joined),
    KnownNodeID =  hash_data(pid_to_list(KnownNodePID), string),
    KnownNode = {KnownNodeID, KnownNodePID},
    NodePID =  lists:nth(1, Remaining),
    NodePID ! {KnownNode},
    timer:sleep(100),
    build_chord(lists:delete(NodePID, Remaining), Joined ++ [NodePID]).

spawn_many(0) ->
    [];
spawn_many(N) ->
        PID = spawn_link(main, join_chord, []),
        [PID] ++ spawn_many(N - 1).
spawn_many(N, max) ->
    PID = spawn_link(main, create_chord, []),
    [PID] ++ spawn_many(N - 1).

create_chord() ->
    Id = hash_data(pid_to_list(self()), string),
    PID = self(),
    N = {Id, PID},
    {Predecessor, Successor} = create(N),
    Finger = fix_fingers(N, Count), %TODO: Change, define count
    run(N, Successor, Predecessor, Finger).

join_chord() ->
    Id = hash_data(pid_to_list(self()), string),
    PID = self(),
    N = {Id, PID},
    receive
        {KnownNode} ->
            {Predecessor, Successor} = join(N, KnownNode),
            Finger = fix_fingers(N, Count), %TODO: Change, define count
            run(N, Successor, Predecessor, Finger)
    end.

run(SelfNode, Successor, Predecessor, Finger) ->
    spawn(main, update, [SelfNode, Successor, Predecessor, Finger]),
    run_loop(SelfNode, Successor, Predecessor, Finger).

run_loop(SelfNode, Successor, Predecessor, Finger) ->
    receive
        {fix_fingers, NewFinger} ->
            run_loop(SelfNode, Successor, Predecessor, NewFinger);
        {update, NewSuccessor} ->
            run_loop(SelfNode, NewSuccessor, Predecessor, Finger);
        {findsuccessor, Id, ReqN} ->
            find_successor(Id, ReqN, SelfNode, Successor, Finger),
            run_loop(SelfNode, Successor, Predecessor, Finger);
        {predecessor, ReqPID} ->
            ReqPID ! {Predecessor},
            run_loop(SelfNode, Successor, Predecessor, Finger);
        {notify, PossiblePredecessor} ->
            NewPredecessor =  notify(SelfNode, PossiblePredecessor, Predecessor),
            run_loop(SelfNode, Successor, NewPredecessor, Finger)
        % {stillWorking} ->

    end.

% Runs update in background for ever node. Each node has a thread (actor)
% Updating its successor and its Finger table.
update(SelfNode, Successor, Predecessor, Finger) ->
    NewSuccessor = stabilize(SelfNode, Successor),
    {SelfID, SelfPID} = SelfNode,
    SelfPID ! {update, NewSuccessor},
    NewFinger = fix_fingers(), %TODO: Change.
    SelfPID ! {fix_fingers, NewFinger},
    timer:sleep(100).

%n.find successor(id)
% Finger is a list of {Id, PID} tuples
find_successor(Id, ReqN, SelfN, Successor, Finger) ->
    {SuccessorID, SuccessorPID} = Successor,
    {SelfID, SelfPID} = SelfN,
    Bool = (Id > SelfID) and (Id =< SuccessorID),
    io:fwrite("Bool inside find_successor ~p for variables ~p ~p ~p", [Bool, Id, SelfID, SuccessorID]),
    if Bool ->
        ReqN ! {Successor};
    true ->
        NewN = closest_preceding_node(Id, SelfID, Finger, ?M),
        NewN ! {findsuccessor, Id, ReqN}
    end.

%n.closest preceding node(id)
% Finger is a list of {Id, PID} tuples
closest_preceding_node(_, SelfID, _, 0) ->
    SelfID;
closest_preceding_node(Id, SelfID, Finger, I) ->
    {FigerID, FingerPID} = lists:nth(I, Finger),
    Bool = (FigerID > SelfID) and (FigerID < Id),
    if Bool ->
        FingerPID;
    true ->
        closest_preceding_node(Id, SelfID, Finger, I-1)
    end.

%n.create() n is calling create
%Assume N is a tuple {ID, PID}
create(N) ->
    Predecessor = {nil, nil},
    Successor = N,
    {Predecessor, Successor}.

%n.join(n′ ) asking node n to join n'
join(N, KnownNode) ->
    Predecessor = {nil, nil},
    {NodeID, NodePID} = N,
    {KnownNodeID, KnownNodePID} = KnownNode,
    KnownNodePID ! {findsuccessor, NodeID, NodePID},
    receive
        {Successor} ->
            ok
    end,
    %Successor also a tuple {ID, PID}
    {Predecessor, Successor}.

%n.stabilize()
%N is a tuple of {ID, PID}
stabilize(N, Successor) ->
    {SuccessorID, SuccessorPID} = Successor,
    {NodeID, NodePID} = N,
    Successor ! {predecessor, self()},
    receive
        {X} -> %successor.predecessor
            ok
    end,
    {XId, XPID} = X,
    Bool = (XId > NodeID) and (XId < SuccessorID),
    if Bool ->
        NewSuccessor = X;
    true ->
        NewSuccessor = Successor
    end,
    % notify(NewSuccessor, N), % successor.notify(n);
    NewSuccessor ! {notify, N},
    NewSuccessor.

% PossiblePredecessor thinks it might be our predecessor
notify(Node, PossiblePredecessor, Predecessor) ->
    {NodeID, NodePID} = Node,
    {PossiblePredecessorID, PossiblePredecessorPID} = PossiblePredecessor,
    {PredecessorID, PredecessorPID} = Predecessor,
    Bool = (PredecessorID == nil) or ((PossiblePredecessorID > PredecessorID) and (PossiblePredecessorID < NodeID)),
    if Bool ->
        NewPredecessor = PossiblePredecessor;
    true ->
        NewPredecessor = Predecessor
    end,
    NewPredecessor.

%fix_fingers - periodically refresh finger table entries

fix_fingers(Node, Count) ->
    if (Count > M) ->
        New_Count = 1;
    true ->
        New_Count = Count,
    New_Count = Count - 1,
    lists:nth(New_Count, Finger)  = find_successor(Node + (math:pow(2, (New_Count - 1)))),
    fix_fingers(Node, New_Count - 1).
    

% called periodically. checks whether predecessor has failed.
check_predecessor(Predecessor) ->
    Predecessor ! {stillWorking},
    receive
        {yes} ->
            ok
    after 
        10000 ->
            io:fwrite("Failure: No response from predecessor\n"),
            Predecessor = nil
    end.


hash_data(Data, number) ->
    StringData = integer_to_list(Data),
    HashBytes = crypto:hash(sha, unicode:characters_to_binary(StringData)),
    HashString = lists:flatten([io_lib:format("~.10.0b", [C]) || <<C>> <= HashBytes]),
    Hash = list_to_integer(HashString),
    Hash;
hash_data(Data, string) ->
    HashBytes = crypto:hash(sha, unicode:characters_to_binary(Data)),
    HashString = lists:flatten([io_lib:format("~.10.0b", [C]) || <<C>> <= HashBytes]),
    Hash = list_to_integer(HashString),
    Hash.
