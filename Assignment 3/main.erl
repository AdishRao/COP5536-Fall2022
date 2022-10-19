-module(main).

-compile(export_all).

-define(M, 160).

start(numNodes, numRequests) ->
    ok.


main(SelfNode, Successor, Predecessor, Finger) ->
    spawn(main, update, [SelfNode, Successor, Predecessor, Finger]),
    run_loop(SelfNode, Successor, Predecessor, Finger).

run_loop(SelfNode, Successor, Predecessor, Finger) ->
    receive
        {fix_fingers, NewFinger} ->
            run_loop(SelfNode, Successor, Predecessor, NewFinger);
        {update, NewSuccessor} ->
            run_loop(SelfNode, NewSuccessor, Predecessor, Finger)
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
    io:fwrite("Bool insidde find_successor ~p for variables ~p ~p ~p", [Bool, Id, SelfID, SuccessorID]),
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
    Successor ! {predecessor},
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
    NewSuccessor ! {notify, NewSuccessor, N},
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
fix_fingers() ->
    % rand:uniform()
    ok.

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
