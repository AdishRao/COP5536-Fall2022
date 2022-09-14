-module(bitcoin_server).

-compile(export_all).

start(K , N_Nodes) ->
    Prefix = lists:concat(lists:duplicate(K, "0")),
    io:fwrite("String : ~p ~p\n", [K, Prefix]),
    spawn_many(N_Nodes, Prefix, node(), self()),
    register(server, self()),
    statistics(runtime),
    {Time, _} = timer:tc(bitcoin_server, bitcoin_server, [0, N_Nodes, Prefix]),
    {_, Time_CPU_Since_Last_Call} = statistics(runtime),
    io:fwrite("Total clock time: ~p\nToal CPU time ~p\n CPU time/ Run Time ~p\n", [Time/1000, Time_CPU_Since_Last_Call, Time_CPU_Since_Last_Call/(Time/1000)]).


    %bitcoin_server(0, N_Nodes, Prefix).

bitcoin_server(100, _, _) ->
    ok;
bitcoin_server(Coins_found, N_Nodes, Prefix) ->
    receive
        {Hash, String, Finder} ->
            io:fwrite("Coin ~p and Hash ~p Found by ~p\n\n", [String, Hash, Finder]),
            Finder ! {mine},
            bitcoin_server(Coins_found+1, N_Nodes, Prefix);
        {node, Node} ->
            io:fwrite("Spawning on Node: ~p\n", [Node]),
            send_code(Node),
            spawn(bitcoin_server, spawn_many, [N_Nodes, Prefix, Node, self()]),
            bitcoin_server(Coins_found, N_Nodes, Prefix)
    end.

send_code(Node) ->
    {Mod, Bin, File} = code:get_object_code(bitcoin_server),
    rpc:multicall([Node], code, load_binary, [Mod, File, Bin]),
    ok.

spawn_many(0, _, _, _) ->
    ok;
spawn_many(N, Prefix, Node, Parent) ->
        spawn(Node, bitcoin_server, find_token, [Prefix, Parent]),
        spawn_many(N - 1, Prefix, Node, Parent).


find_token(Prefix, Parent) ->
    String = generate_random_string(),
    Hash = calculate_hash(String),
    Coin = string:find(Hash, Prefix) =:= Hash,
    if Coin == true ->
           Parent ! {Hash, String, self()},
           receive
                {mine} ->
                    find_token(Prefix, Parent)
            after
                10000 ->
                    ok
            end;
    true ->
        find_token(Prefix, Parent)
    end.

calculate_hash(String) ->
    Hash =
        io_lib:format("~64.16.0b",
                      [binary:decode_unsigned(
                           crypto:hash(sha256, String))]),
    Hash.

generate_random_string() ->
    Base64_string =
        base64:encode(
            crypto:strong_rand_bytes(72)),
    Gen_string = binary_to_list(Base64_string),
    Final_string = string:join(["adishsomeshwarao", Gen_string], ";"),
    Final_string.
