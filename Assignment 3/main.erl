-module(main).

-compile(export_all).

-define(M, 160).

start(numNodes, numRequests) ->
    ok.



%n.find successor(id)
find_successor(Id, ReqN, SelfN, Successor, Finger) ->
    Bool = (Id > SelfN) and (Id =< Successor),
    if Bool ->
        ReqN ! {Successor};
    true ->
        NewN = closest_preceding_node(Id, SelfN, Finger),
        NewN ! {findsuccessor, Id, ReqN}
    end.

closest_preceding_node(_, _, _) ->
  ok.

%n.create() n is calling create
create(N) ->
    Predecessor = nil,
    Successor = N,
    {Predecessor, Successor}.

%n.join(n′ ) asking node n to join n'
join(N, KnownNode) ->
    Predecessor = nil,
    KnownNode ! {findsuccessor, N, self()},
    receive
        {Successor} ->
            ok
    end,
    {Predecessor, Successor}.

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
