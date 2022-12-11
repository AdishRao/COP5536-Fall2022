-module(client).

-compile(export_all).

start(Server_node) ->
    net_adm:ping(Server_node),
    New_or_Current_User = string:chomp(io:get_line("1. Login\n2. Register\n")),
    if New_or_Current_User == "1" ->
        io:fwrite("Enter Login Details\n"),
        login(Server_node);
    true ->
        io:fwrite("Enter Register Details\n"),
        register(Server_node)  
    end,
    start_listening(Server_node).

start_listening() ->
    receive
        {tweet_response, error} ->
            io:fwrite("Error tweeting\n");
        {tweet_response, ok} ->
            io:fwrite("Tweet successful\n");
        {query_response, Tweet, TweetId, Tweeter} ->
            io:fwrite("Query Response \nTweet\t~p \nTweeter \t~p \nTweetID\t~p\n", [Tweet, Tweeter, TweetId]);
        {new_tweet, Tweet, Tweeter, TweetId} ->
            io:fwrite("New Tweet \nTweet\t~p \nTweeter \t~p \nTweetID\t~p\n", [Tweet, Tweeter, TweetId])
    end,
    start_listening().

start_listening(Server_node) ->
    receive
        {login_response, Username, ok} ->
            io:fwrite("Login Successful\n"),
            io:fwrite("Welcome back!\n"),
            spawn_link(client, start_tweeting, [Server_node, Username]),
            start_listening();
        {register_response, Username, ok} ->
            io:fwrite("Registration Successful\n"),
            io:fwrite("Welcome to Twitter!\n"),
            spawn_link(client, start_tweeting, [Server_node, Username]),
            start_listening();
        {login_response, error} ->
            io:fwrite("Login Error\n"),
            start(Server_node);
        {register_response, error} ->
            io:fwrite("Registration error\n"),
            start(Server_node)
    end.

start_tweeting(Server_node, Username) ->
    Operation = string:chomp(io:get_line("1.tweet \n2.query \n3.subscribe \n4.retweet\n")),
    case Operation of 
        "1" -> tweet(Server_node, Username);
        "2" -> query(Server_node, Username);
        "3" -> subscribe(Server_node, Username);
        "4" -> retweet(Server_node, Username);
        _ -> ok
    end,
    start_tweeting(Server_node, Username).

register(Server_node) ->
    Username = string:chomp(io:get_line("Enter Username\n")),
    Password = string:chomp(io:get_line("Enter Password\n")),
    {server, Server_node} ! {client_register, self(), Username, Password}.
    
login(Server_node)->
    Username = string:chomp(io:get_line("Enter Username\n")),
    Password = string:chomp(io:get_line("Enter Password\n")),
    {server, Server_node}  ! {client_login, self(), Username, Password}.

tweet(Server_node, Username) ->
    Tweet = string:chomp(io:get_line("What would you like to say?\n")),
    {server, Server_node}  ! {client_tweet, Username, Tweet}.

subscribe(Server_node, Username) ->
    New_subscription_raw = string:chomp(io:get_line("What are you subscribing to?\n")),
    New_subscription = process_string(New_subscription_raw),
    {server, Server_node}  ! {client_subscribe_to, Username, New_subscription}.

query(Server_node, Username)->
    Query_raw = string:chomp(io:get_line("What/Whom do you want to know more about?\n")),
    Query = process_string(Query_raw),
    {server, Server_node}  ! {client_query, Username, Query}.

process_string(String) ->
    case string:prefix(String, "#") of
        nomatch ->
            case string:prefix(String, "@") of
                nomatch ->
                    ReturnString = String;
                StringWithoutMention ->
                    ReturnString =  string:concat("mention_", StringWithoutMention)
            end;
        StringWithoutHash ->
            ReturnString =  string:concat("hash_", StringWithoutHash)
    end,
    ReturnString.

retweet(Server_node, Username)->
    {ok, TweetId} = io:read("Enter the tweet id you'd like to retweet.\n"),
    {server, Server_node} ! {client_retweet, Username, TweetId}.