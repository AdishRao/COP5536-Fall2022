-module(client).

% ignore
-compile(export_all).
% -export([start/0, start_listening/0, start_tweeting/0, register/0, login/0, tweet/1, query/1, retweet/1, subscribe/1]).

start(Server_node) ->
    net_adm:ping(Server_node),
    {ok, New_or_Current_User} = io:read("Login or Register: "),
    Pattern = "login",
    if New_or_Current_User == Pattern->
        io:fwrite("Inside login if"),
        login(Server_node);
    true ->
        io:fwrite("Inside else"),
        register(Server_node)  
    end,
    start_listening(Server_node).
    

start_listening(Server_node) ->
    receive
        {tweet_response, error} ->
            io:fwrite("Error tweeting");
        {tweet_response, ok} ->
            io:fwrite("Tweet successful");
        {query_response, Tweet, TweetId, Tweeter} ->
            io:swrite("Query Response | Tweet, TweetID, Tweeter : \n", [Tweet, TweetId, Tweeter]);
        {login_response, error} ->
            io:fwrite("Login Error"),
            start(Server_node);
        {register_response, error} ->
            io:fwrite("Registration error"),
            start(Server_node);
        {login_response, Username, ok} ->
            io:fwrite("Login Successful"),
            io:fwrite("Welcome back!"),
            start_tweeting(Server_node, Username);
        {register_response, Username, ok} ->
            io:fwrite("Registration Successful"),
            io:fwrite("Welcome to Twitter!"),
            start_tweeting(Server_node, Username)
    end.

start_tweeting(Server_node, Username) ->
    %To be put in a while loop 
    {ok, Operation} = io:read("You can now tweet, query, subscribe or retweet.
                                What would you like to do?"),
    case Operation of 
        tweet -> tweet(Server_node, Username);
        subscribe -> subscribe(Server_node, Username);
        query -> query(Server_node, Username);
        retweet -> retweet(Server_node, Username)
    end.

% Capital letter input causes error
register(Server_node) ->
    {ok, Username} = io:read("Enter Username: "),
    {ok, Password} = io:read("Enter Password: "),
    {server, Server_node} ! {client_register, self(), Username, Password}.
    
login(Server_node)->
    {ok, Username} = io:read("Enter Username: "),
    % io:format("~w~n",[Username]),
    {ok, Password} = io:read("Enter Password: "),
    % io:format("~w~n",[Password]),
    {server, Server_node}  ! {client_login, self(), Username, Password}.

tweet(Server_node, Username) ->
    {ok, Tweet} = io:read("What would you like to say? "),
    {server, Server_node}  ! {client_tweet, Username, Tweet}.

subscribe(Server_node, Username) ->
    New_subscription = io:read("What are you subscribing to? "),
    {server, Server_node}  ! {client_subscribe_to, Username, New_subscription}.

query(Server_node, Username)->
    Query = io:read("What/Whom do you want to know more about? "),
    {server, Server_node}  ! {client_query, Username, Query}.

retweet(Server_node, Username)->
    {ok, Tweet, Tweeter} = io:read("Enter the tweet and the tweeter's name whom you'd like to retweet"),
    {server, Server_node}  ! {client_retweet, Username, Tweet, Tweeter}.
    
        