-module(client).

% ignore
-compile(export_all).
% -export([start/0, start_listening/0, start_tweeting/0, register/0, login/0, tweet/1, query/1, retweet/1, subscribe/1]).

start(Server_node) ->
    net_adm:ping(Server_node),
    New_or_Current_User = string:chomp(io:get_line("Login or Register: ")),
    if New_or_Current_User == "login" ->
        io:fwrite("Inside login if\n"),
        login(Server_node);
    true ->
        io:fwrite("Inside else\n"),
        register(Server_node)  
    end,
    start_listening(Server_node).
    

start_listening() ->
    receive
        {tweet_response, error} ->
            io:fwrite("Error tweeting");
        {tweet_response, ok} ->
            io:fwrite("Tweet successful");
        {query_response, Tweet, TweetId, Tweeter} ->
            io:fwrite("Query Response | Tweet\t~p\n, Tweeter \t~p\n, TweetID\t~p\n", [Tweet, Tweeter, TweetId]);
        {new_tweet, Tweet, Tweeter, TweetId} ->
            io:fwrite("New Tweet | Tweet\t~p\n, Tweeter \t~p\n, TweetID\t~p\n", [Tweet, Tweeter, TweetId])
    end.
start_listening(Server_node) ->
    receive
        {login_response, Username, ok} ->
            io:fwrite("Login Successful\n"),
            io:fwrite("Welcome back!\n"),
            spawn_link(client, start_tweeting, [Server_node, Username]),
            start_listening();
        {register_response, Username, ok} ->
            io:fwrite("Registration Successful"),
            io:fwrite("Welcome to Twitter!"),
            spawn_link(client, start_tweeting, [Server_node, Username]),
            start_listening();
        {login_response, error} ->
            io:fwrite("Login Error"),
            start(Server_node);
        {register_response, error} ->
            io:fwrite("Registration error"),
            start(Server_node)
    end.

start_tweeting(Server_node, Username) ->
    Operation = string:chomp(io:get_line("You can now tweet, query, subscribe or retweet. What would you like to do?\n")),
    case Operation of 
        "tweet" -> tweet(Server_node, Username);
        "subscribe" -> subscribe(Server_node, Username);
        "query" -> query(Server_node, Username);
        "retweet" -> retweet(Server_node, Username)
    end,
    start_tweeting(Server_node, Username).

register(Server_node) ->
    Username = string:chomp(io:get_line("Enter Username: ")),
    Password = string:chomp(io:get_line("Enter Password: ")),
    {server, Server_node} ! {client_register, self(), Username, Password}.
    
login(Server_node)->
    Username = string:chomp(io:get_line("Enter Username: ")),
    Password = string:chomp(io:get_line("Enter Password: ")),
    {server, Server_node}  ! {client_login, self(), Username, Password}.

tweet(Server_node, Username) ->
    Tweet = string:chomp(io:get_line("What would you like to say? ")),
    {server, Server_node}  ! {client_tweet, Username, Tweet}.

subscribe(Server_node, Username) ->
    New_subscription_raw = string:chomp(io:get_line("What are you subscribing to? ")),
    New_subscription = process_string(New_subscription_raw),
    {server, Server_node}  ! {client_subscribe_to, Username, New_subscription}.

query(Server_node, Username)->
    Query_raw = string:chomp(io:get_line("What/Whom do you want to know more about? ")),
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
    {ok, TweetId} = io:read("Enter the tweet id you'd like to retweet"),
    {server, Server_node} ! {client_retweet, Username, TweetId}.