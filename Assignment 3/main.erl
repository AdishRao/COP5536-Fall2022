-module(main).

-compile(export_all).

-define(M, 160).

-define(maxval, math:pow(2, ?M)).

start(NumNodes, NumRequests) ->
    register(server, self()),
    PIDS = spawn_many(NumNodes, NumRequests, NumNodes, max),
    build_chord(lists:sublist(PIDS, 2, NumNodes), [lists:nth(1, PIDS)]),
    Avg = cumulate(NumNodes*NumRequests, NumNodes*NumRequests, 0),
    io:fwrite("\nAverage is ~p\n", [Avg]).


cumulate(0, Div, Sum) ->
    Sum/Div;
cumulate(N, Div, Sum) ->
    receive
        {Count} ->
            %io:fwrite("####### FOUND Count ~p\n", [Count]),
            io:fwrite(".", []),
            Ret = cumulate(N-1, Div, Sum+Count),
            Ret
    end.

build_chord([], _) ->
    ok;
build_chord(Remaining, Joined) ->
    KnownNodePID = lists:nth(rand:uniform(length(Joined)), Joined),
    KnownNodeID =  hash_data(pid_to_list(KnownNodePID), string),
    KnownNode = {KnownNodeID, KnownNodePID},
    NodePID =  lists:nth(1, Remaining),
    NodePID ! {KnownNode},
    timer:sleep(5000),
    build_chord(lists:delete(NodePID, Remaining), Joined ++ [NodePID]).

spawn_many(0, _, _) ->
    [];
spawn_many(N,NumRequests, NumNodes) ->
        PID = spawn(main, join_chord, [NumRequests, NumNodes]),
        [PID] ++ spawn_many(N - 1, NumRequests, NumNodes).
spawn_many(N, NumRequests, NumNodes, max) ->
    PID = spawn(main, create_chord, [NumRequests, NumNodes]),
    [PID] ++ spawn_many(N - 1, NumRequests, NumNodes).

create_chord(NumRequests, NumNodes) ->
    Id = hash_data(pid_to_list(self()), string),
    PID = self(),
    N = {Id, PID},
    {Predecessor, Successor} = create(N),
    Finger = lists:duplicate(?M, Successor),
    run(N, Successor, Predecessor, Finger, NumRequests, NumNodes).

join_chord(NumRequests, NumNodes) ->
    Id = hash_data(pid_to_list(self()), string),
    PID = self(),
    N = {Id, PID},
    receive
        {KnownNode} ->
            {Predecessor, Successor} = join(N, KnownNode),
            Finger = lists:duplicate(?M, Successor),
            run(N, Successor, Predecessor, Finger, NumRequests, NumNodes)
    end.

run(SelfNode, Successor, Predecessor, Finger, NumRequests, NumNodes) ->
    Updater = spawn(main, update, [SelfNode, Successor, Predecessor, Finger, (?M + 1)]),
    spawn(main, makerequest, [SelfNode, NumRequests, NumNodes, wait]),
    run_loop(SelfNode, Successor, Predecessor, Finger, Updater).

run_loop(SelfNode, Successor, Predecessor, Finger, Updater) ->
    % {SelfID, _} = SelfNode,
    % {SuccessorID, _} = Successor,
    % {PredecessorID, _} = Predecessor,
    % io:fwrite("I am ~p and my successor is ~p and my predecessor is ~p \n\n", [SelfID, SuccessorID, PredecessorID]),
    receive
        {update, NewSuccessor} ->
            run_loop(SelfNode, NewSuccessor, Predecessor, Finger, Updater);

        {findsuccessor, Id, ReqN, JumpCount} ->
            find_successor(Id, ReqN, SelfNode, Successor, Finger, JumpCount+1),
            run_loop(SelfNode, Successor, Predecessor, Finger, Updater);

        {predecessor, ReqPID} ->
            ReqPID ! {mypredecessor, Predecessor},
            run_loop(SelfNode, Successor, Predecessor, Finger, Updater);

        {notify, PossiblePredecessor} ->
            NewPredecessor =  notify(SelfNode, PossiblePredecessor, Predecessor),
            if NewPredecessor /= Predecessor ->
                Updater ! {newpredecessor, NewPredecessor};
            true ->
                ok
            end,
            run_loop(SelfNode, Successor, NewPredecessor, Finger, Updater);

        {fix_fingers, NewFinger} ->
            run_loop(SelfNode, Successor, Predecessor, NewFinger, Updater);

        {stillWorking, Req} ->
            Req ! {yes},
            run_loop(SelfNode, Successor, Predecessor, Finger, Updater);

        {predDead} ->
            NewPredecessor = {nil, nil},
            Updater ! {newpredecessor, NewPredecessor},
            run_loop(SelfNode, Successor, NewPredecessor, Finger, Updater)
    end.

% Makes a request every 1 second for its parent main node
makerequest(_, 0, _) ->
    ok;
makerequest(SelfNode, NumRequests, NumNodes) ->
    timer:sleep(1000),
    {_, SelfPID} = SelfNode,
    Id = rand:uniform(trunc(?maxval)),
    SelfPID ! {findsuccessor, Id, self(), -1},
    receive
        {successor_found, _, JumpCount} ->
            server ! {JumpCount}
    end,
    makerequest(SelfNode, NumRequests - 1, NumNodes).
makerequest(SelfNode, NumRequests, NumNodes, wait) ->
    timer:sleep(NumNodes*500 + 120000),
    makerequest(SelfNode, NumRequests, NumNodes).

% Runs update in background for ever node. Each node has a thread (actor)
% Updating its successor and its Finger table.
update(SelfNode, Successor, Predecessor, Finger, Count) ->
    {NewSuccessor, RetFinger} = stabilize(SelfNode, Successor, Finger),
    {_, SelfPID} = SelfNode,
    if NewSuccessor /= Successor ->
        SelfPID ! {update, NewSuccessor};
    true ->
        ok
    end,
    NewFinger = fix_fingers(SelfNode, ?M, RetFinger),
    SelfPID ! {fix_fingers, NewFinger},
    receive
        {newpredecessor, NewPredecessor} ->
            ok
    after
        0 ->
            NewPredecessor = Predecessor
    end,
    timer:sleep(500),
    update(SelfNode, NewSuccessor, NewPredecessor, NewFinger, Count).

%n.find successor(id)
% Finger is a list of {Id, PID} tuples math:log2
find_successor(Id, ReqN, SelfN, Successor, Finger, JumpCount) ->
    {SuccessorID, _} = Successor, % SuccessorPID = _
    {SelfID, SelfPID} = SelfN,

    if (SelfID == SuccessorID) -> % First node who created chord
        ReqN ! {successor_found, Successor, JumpCount};
    SuccessorID > SelfID -> % Successor is next on chord
        Bool = (Id > SelfID) and (Id =< SuccessorID),
        if Bool ->
            ReqN ! {successor_found, Successor, JumpCount};
        true ->
            NewN = closest_preceding_node(Id, SelfID, SelfPID, Finger, ?M),
            NewN ! {findsuccessor, Id, ReqN, JumpCount}
        end;
    SelfID > SuccessorID ->
        Bool = ((Id > SelfID) and (Id =< ?maxval)) or ((Id >= 0) and (Id =< SuccessorID)),
        if Bool ->
            ReqN ! {successor_found, Successor, JumpCount};
        true ->
            NewN = closest_preceding_node(Id, SelfID, SelfPID, Finger, ?M),
            NewN ! {findsuccessor, Id, ReqN, JumpCount}
        end;
    true ->
        ok
    end.

%n.closest preceding node(id)
% Finger is a list of {Id, PID} tuples math:log2
closest_preceding_node(_, _, SelfPID, _, 0) ->
    SelfPID;
closest_preceding_node(Id, SelfID, SelfPID, Finger, I) -> % First I = ?M
    {FigerID, FingerPID} = lists:nth(I, Finger),
    if Id > SelfID ->
        Bool = (FigerID > SelfID) and (FigerID < Id),
        if Bool ->
            FingerPID;
        true ->
            RetPID = closest_preceding_node(Id, SelfID, SelfPID, Finger, I-1),
            RetPID
        end;
    SelfID > Id ->
        Bool = ((FigerID > SelfID) and (FigerID =< ?maxval)) or ((FigerID >= 0) and (FigerID < Id)),
        if Bool ->
            FingerPID;
        true ->
            RetPID = closest_preceding_node(Id, SelfID, SelfPID, Finger, I-1),
            RetPID
        end;
    true ->
        RetPID = closest_preceding_node(Id, SelfID, SelfPID, Finger, I-1),
        RetPID
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
    {_, KnownNodePID} = KnownNode, % KnownNodeID = _
    KnownNodePID ! {findsuccessor, NodeID, NodePID, 0},
    receive
        {successor_found, Successor, _} ->
            ok
    end,
    %Successor also a tuple {ID, PID}
    {Predecessor, Successor}.

%n.stabilize()
%N is a tuple of {ID, PID}
stabilize(N, Successor, Finger) ->
    {SuccessorID, SuccessorPID} = Successor, % SuccessorPID = _
    {NodeID, _} = N, % NodePID = _
    SuccessorPID ! {predecessor, self()},
    receive
        {mypredecessor, X} -> %successor.predecessor
            ok
    end,
    {XId, _} = X,
    if XId == nil ->
        NewSuccessor = Successor;
    SuccessorID == NodeID ->
        RetFinger = lists:duplicate(?M, X),
        NewSuccessor = X,
        {NewSuccessor, RetFinger};
    SuccessorID > NodeID ->
        Bool = (XId > NodeID) and (XId < SuccessorID),
        if Bool ->
            NewSuccessor = X;
        true ->
            NewSuccessor = Successor
        end;
    NodeID > SuccessorID ->
        Bool = ((XId > NodeID) and (XId =< ?maxval)) or ((XId >= 0) and (XId < SuccessorID)),
        if Bool ->
            NewSuccessor = X;
        true ->
            NewSuccessor = Successor
        end
    end,
    % successor.notify(n);
    {_, NewSuccessorPID} = NewSuccessor,
    NewSuccessorPID ! {notify, N},
    {NewSuccessor, Finger}.

% PossiblePredecessor thinks it might be our predecessor
notify(Node, PossiblePredecessor, Predecessor) ->
    {NodeID, _} = Node, % NodePID = _
    {PossiblePredecessorID, _} = PossiblePredecessor, % PossiblePredecessorPID = _
    {PredecessorID, _} = Predecessor, % PredecessorPID = _
    if PossiblePredecessorID == NodeID -> % Can happen when only 2 nodes.
        NewPredecessor = Predecessor;
    PredecessorID == nil -> % Newly joined the chord
        NewPredecessor = PossiblePredecessor;
    PredecessorID < NodeID -> % Regular, node is behind current node
        Bool = ((PossiblePredecessorID > PredecessorID) and (PossiblePredecessorID < NodeID)),
        if Bool ->
            NewPredecessor = PossiblePredecessor;
        true ->
            NewPredecessor = Predecessor
        end;
    PredecessorID > NodeID ->
        Bool = ((PossiblePredecessorID > PredecessorID) and (PossiblePredecessorID =< ?maxval)) or ((PossiblePredecessorID >= 0) and (PossiblePredecessorID < NodeID)),
        if Bool ->
            NewPredecessor = PossiblePredecessor;
        true ->
            NewPredecessor = Predecessor
        end
    end,
    NewPredecessor.

fix_fingers(_, 0, Finger) ->
    Finger;
fix_fingers(Node, Count, Finger) ->
    timer:sleep(10),
    {NodeID, NodePID} = Node,
    NodePID ! {findsuccessor, (trunc(NodeID + math:pow(2, (Count - 1))) rem trunc(?maxval)), self(), 0},
    receive
        {successor_found, Successor, _} ->
            ok
    after 
        1000 ->
            Successor = lists:nth(Count, Finger)
    end,
    NewFinger = fix_fingers(Node, Count - 1, lists:sublist(Finger,Count-1) ++  [Successor] ++ lists:nthtail(Count,Finger)),
    NewFinger.

% called periodically. checks whether predecessor has failed.
check_predecessor(Predecessor) ->
    {_, PredecessorPID} = Predecessor,
    PredecessorPID ! {stillWorking, self()},
    receive
        {yes} ->
            ok
    after 
        500 ->
            io:fwrite("Failure: No response from predecessor\n"),
            dead
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
    Hash rem trunc(?maxval).
