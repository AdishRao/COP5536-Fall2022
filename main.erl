-module(main).

-export([main/1]).

main(Args) ->
    [K, IP | _] = Args,
    Prefix =
        lists:concat(
            lists:duplicate(K, "0")),
    io:fwrite("String : ~p ~p ~p\n", [K, IP, Prefix]),
    find_token(Prefix).

find_token(Prefix) ->
    String = generate_random_string(),
    Hash = calculate_hash(String),
    Coin = string:find(Hash, Prefix) =:= Hash,
    if Coin == true ->
           io:fwrite("Coin? : ~p\n", [Coin]),
           io:fwrite("Hash : ~p\n", [Hash]),
           io:fwrite("Final String : ~p\n", [String]);
       Coin /= true ->
           false
    end,
    find_token(Prefix).

calculate_hash(String) ->
    Hash =
        io_lib:format("~64.16.0b",
                      [binary:decode_unsigned(
                           crypto:hash(sha256, String))]),
    Hash.

generate_random_string() ->
    Base64_string =
        base64:encode(
            crypto:strong_rand_bytes(24)),
    Gen_string = binary_to_list(Base64_string),
    Final_string = string:join(["adishsomeshwarao", Gen_string], ";"),
    Final_string.

