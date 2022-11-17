-module(client).

% ignore
-compile(export_all).

start() ->
    {ok, New_or_Current_User} = io:read("Login or Sign Up: "),
    if New_or_Current_User == "Login"->
        Login();
    true ->
        Register();
    end.
    start_listening()
    

start_listening() ->
    receive
        {tweet_response, error} ->
            io:fwrite("Error tweeting");
        {tweet_response, ok} ->
            io:fwrite("Tweet successful");
        {query_response, Tweet, TweetId, Tweeter} ->
            io:swrite("Query Response | Tweet, TweetID, Tweeter : \n", [Tweet, TweetId, Tweeter]);
        {login_response, error} ->
            io:fwrite("Login Error"),
            start();
        {register_response, error} ->
            io:fwrite("Registration error"),
            start();
        {login_response, Username, ok} ->
            io:fwrite("Login Successful"),
            io:fwrite("Welcome back!"),
            Perform_operations(Username);
        {register_response, Username, ok} ->
            io:fwrite("Registration Successful"),
            io:fwrite("Welcome to Twitter!"),
            Start_tweeting(Username);
    end;

Start_tweeting(Username) ->
    %To be put in a while loop 
    {ok, Operation} = io:read("You can now Tweet, Query, Subscribe or Retweet.
                                What would you like to do?"),
    case Operation of ->
        "Tweet" -> tweet(Username);
        "Subscribe" -> subscribe(Username);
        "Query" -> query(Username);
        "Retweet" -> retweet(Username);
    end



register() ->
    {ok, Username} = io:read("Enter Username: "),
    {ok, Password} = io:read("Enter Password"),
    server ! {client_register, self(), Username, Password};
    
logIn()->
    {ok, Username} = io:read("Enter Username: "),
    {ok, Password} = io:read("Enter Password"),
    server ! {client_login, self(), Username, Password};

tweet(Username) ->
    {ok, Tweet} -> io:read("What would you like to say? "),
    server ! {client_tweet, Username, Tweet};

subscribe(Username) ->
    New_subscription -> io:read("What are you subscribing to? "),
    server ! {client_subscribe_to, Username, New_subscription};

query(Username)->
    Query -> io:read("What/Whom do you want to know more about? "),
    server ! {client_query, Username, Query};

retweet(Username)->
    Tweet, Tweeter -> io:read("Enter the tweet and the tweeter's name whom you'd like to retweet");
    server ! {client_retweet, Username, Tweet, Tweeter};
    
        