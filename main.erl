-module(main).
-compile(export_all).

main(Args) ->
    [K, N_Nodes | Nodes ] = Args,
    Prefix =
        lists:concat(
            lists:duplicate(K, "0")),
    io:fwrite("String : ~p ~p ~p\n", [K, Nodes, Prefix]),
    [net_adm:ping(Node) || Node <- Nodes],
    {Mod, Bin, File} = code:get_object_code(main),
    rpc:multicall(code, load_binary, [Mod, File, Bin]),
    Coins_found = 0,
    if Nodes /= [] ->
           [spawn_many(N_Nodes, Prefix, Node) || Node <- Nodes];
       true ->
           ok
    end,
    spawn_many(N_Nodes, Prefix, node()),
    print_coins(Coins_found).

print_coins(100) ->
    ok;

print_coins(Coins_found) ->
    receive
        {Hash, String, Finder} ->
            io:fwrite("Coin ~p and Hash ~p Found by ~p\n\n", [String, Hash, Finder]),
            New_coin_count = increment_coins_found(Coins_found),
            print_coins(New_coin_count)
    end.

increment_coins_found(Coins_found) ->
    New_coin_count = Coins_found + 1,
    New_coin_count.


spawn_many(0, _, _) ->
    ok;
spawn_many(N, Prefix, Node) ->
        spawn(Node, main, find_token, [Prefix, self()]),
        spawn_many(N - 1, Prefix, Node).


find_token(Prefix, Parent) ->
    String = generate_random_string(),
    Hash = calculate_hash(String),
    Coin = string:find(Hash, Prefix) =:= Hash,
    if Coin == true ->
           Parent ! {Hash, String, self()};
       true ->
           ok
    end,
    find_token(Prefix, Parent).

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
