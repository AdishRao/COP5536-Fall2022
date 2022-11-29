-module(client_automated).

-compile(export_all).

-define(UseZipf, true).

start(Server_node, NumUsers) ->
    start_spawning(Server_node, NumUsers, NumUsers).

start_spawning(_, 0, _) ->
    ok;
start_spawning(Server_node, User, MaxUsers) ->
    User_string = integer_to_list(User),
    spawn(client_automated, start, [Server_node, User_string, MaxUsers]),
    start_spawning(Server_node, User-1, MaxUsers).

start(Server_node, YourUsername, MaxUsers) ->
    net_adm:ping(Server_node),
    put(max, MaxUsers),
    put(server, Server_node),
    register_user(Server_node, YourUsername),
    start_listening(Server_node).

start_listening() ->
    receive
        {tweet_response, error} ->
            ok;
        {tweet_response, ok} ->
            ok;
        {new_tweet, _, _, TweetId} ->
            Prob = rand:uniform(),
            if ?UseZipf ->
                ZipProb = zipf(get(username));
            true ->
                ZipProb = 0
            end,
            if Prob =< (0.1 + ZipProb) ->
                retweet(get(server), get(username), TweetId);
            true ->
                ok
            end
    end,
    start_listening().

start_listening(Server_node) ->
    receive
        {register_response, Username, ok} ->
            put(username, Username),
            spawn_link(client_automated, start_tweeting_setup, [Server_node, Username, get(max)]),
            start_listening();
        {register_response, error} ->
            io:fwrite("Registration error\n"),
            exit(register_failure)
    end.

start_tweeting_setup(Server_node, Username, MaxUsers) ->
    put(max, MaxUsers),
    timer:sleep(1000),
    if ?UseZipf ->
        Num_Subs = ceil(zipf(Username)*get(max));
    true ->
        Max = get(max) div 4,
        Num_Subs = rand:uniform(Max)
    end,
    subscribe_loop(Server_node, Username, Num_Subs),
    start_tweeting(Server_node, Username).

subscribe_loop(_, _, 0) ->
    ok;
subscribe_loop(Server_node, Username, Num_Subs) ->
    MySub = integer_to_list(rand:uniform(get(max))),
    subscribe(Server_node, MySub, Username),
    subscribe_loop(Server_node, Username, Num_Subs-1).

start_tweeting(Server_node, Username) ->
    Prob = rand:uniform(),
    if ?UseZipf ->
        ZipProb = zipf(Username);
    true ->
        ZipProb = 0
    end,
    if Prob =< (0.4 + 3*ZipProb) ->
        tweet(Server_node, Username);
    true ->
        ok
    end,
    timer:sleep(500),
    start_tweeting(Server_node, Username).

register_user(Server_node, YourUsername) ->
    {server, Server_node} ! {client_register, self(), YourUsername, YourUsername}.

tweet(Server_node, Username) ->
    Tweet = generate_random_string(),
    {server, Server_node}  ! {client_tweet, Username, Tweet}.

subscribe(Server_node, Username, New_subscription) ->
    {server, Server_node}  ! {client_subscribe_to, Username, New_subscription}.

retweet(Server_node, Username, TweetId)->
    {server, Server_node} ! {client_retweet, Username, TweetId}.


zipf(Username)->
    Num = (1/list_to_integer(Username)),
    Den = math:log(get(max)) + 0.5772156649,
    Zipf = (Num/Den),
    Zipf.

generate_random_string() ->
    Base64_string =
        base64:encode(
            crypto:strong_rand_bytes(72)),
    Gen_string = binary_to_list(Base64_string),
    Gen_string.