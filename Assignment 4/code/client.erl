-module(client).

% ignore
-compile(export_all).

start() ->
    % As per input, function to be called.

start_listening() ->
    receive
        {tweet_response, error} ->
            io:fwrite("Error tweeting");
        {tweet_response, ok} ->
            io:fwrite("Tweet successful");
        {query_response, Tweet, TweetId, Tweeter} ->
            io:swrite("Query Response | Tweet, TweetID, Tweeter : \n", [Tweet, TweetId, Tweeter]);
        %Add unsuccessful query response: No record found
        {login_response, error} ->
            io:fwrite("Login Error");
        {login_response, ok} ->
            io:fwrite("Login Successful");
        {register_response, error} ->
            io:fwrite("Registration Successful");
        {register_response, ok} ->
            io:fwrite("Error");


Register(username, password) ->
    server ! {client_register, self(), Username, Password},
    start_listening();

LogIn(username, password)->
    server ! {client_login, self(), Username, Password},
    start_listening();

Tweet(username, Tweet) ->
    server ! {client_tweet, Username, Tweet},
    start_listening();

Subscribe(username, New_subscription) ->
    server ! {client_subscribe_to, Username, New_subscription},
    start_listening();

Query(username, Query)->
    server ! {client_query, Username, Query},
    start_listening();

Retweet(username, Tweet, Tweeter)->
    server ! {client_retweet, Username, Tweet, Tweeter},
    start_listening();
    
        