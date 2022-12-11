-module(my_client).

-export([start/0, user_interact/1]).

start() ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
    timer:apply_interval(5000, gun, ws_send, [ConnPid, StreamRef, ping]),
    New_or_Current_User = string:chomp(io:get_line("1. Login\n2. Register\n")),
    if New_or_Current_User == "1" ->
        io:fwrite("Enter Login Details\n"),
        login(ConnPid, StreamRef);
    true ->
        io:fwrite("Enter Register Details\n"),
        register_user(ConnPid, StreamRef)  
    end.

register_user(ConnPid, StreamRef) ->
    Username = list_to_binary(string:chomp(io:get_line("Enter Username\n"))),
    Password = list_to_binary(string:chomp(io:get_line("Enter Password\n"))),
    GetMessage = #{<<"Query">> => <<"register">>, <<"Username">> => Username, <<"Password">> => Password},
    EncodedMessage = jsone:encode(GetMessage),
    gun:ws_send(ConnPid, StreamRef, {text, EncodedMessage}),
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, _Frame}} ->
            DecodeData = jsone:decode(_Frame),
            ResgisterResponse = binary_to_list(maps:get(<<"response">>, DecodeData)),
            case ResgisterResponse of
                "ok" ->
                    RetUsername = maps:get(<<"username">>, DecodeData),
                    spawn(my_client, user_interact, [self()]),
                    start_listening(RetUsername, ConnPid, StreamRef);
                "error" ->
                    io:fwrite("ERROR"),
                    register_user(ConnPid, StreamRef)
            end
    end.
    
login(ConnPid, StreamRef)->
    Username = list_to_binary(string:chomp(io:get_line("Enter Username\n"))),
    Password = list_to_binary(string:chomp(io:get_line("Enter Password\n"))),
    GetMessage = #{<<"Query">> => <<"login">>, <<"Username">> => Username, <<"Password">> => Password},
    EncodedMessage = jsone:encode(GetMessage),
    gun:ws_send(ConnPid, StreamRef, {text, EncodedMessage}),
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, _Frame}} ->
            DecodeData = jsone:decode(_Frame),
            LoginResponse = binary_to_list(maps:get(<<"response">>, DecodeData)),
            case LoginResponse of
                "ok" ->
                    RetUsername = maps:get(<<"username">>, DecodeData),
                    spawn(my_client, user_interact, [self()]),
                    start_listening(RetUsername, ConnPid, StreamRef);
                "error" ->
                    io:fwrite("ERROR"),
                    login(ConnPid, StreamRef)
            end
            
    end.

start_listening(Username, ConnPid, StreamRef) ->
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, _Frame}} ->
            DecodeData = jsone:decode(_Frame),
            ResponseType = binary_to_list(maps:get(<<"response">>, DecodeData)),
            case ResponseType of
                "tweet_response" ->
                    Status = binary_to_list(maps:get(<<"status">>, DecodeData)),
                    case Status of
                        "error" ->
                            io:fwrite("Error tweeting\n");
                        "ok" ->
                            io:fwrite("Tweet successful\n")
                    end;
                "query_response" ->
                    Tweet = binary_to_list(maps:get(<<"tweet">>, DecodeData)),
                    TweetId = (maps:get(<<"tweetid">>, DecodeData)),
                    Tweeter = binary_to_list(maps:get(<<"tweeter">>, DecodeData)),
                    io:fwrite("Query Response \nTweet\t~p \nTweeter \t~p \nTweetID\t~p\n", [Tweet, Tweeter, TweetId]);
                "new_tweet" ->
                    Tweet = binary_to_list(maps:get(<<"tweet">>, DecodeData)),
                    TweetId = (maps:get(<<"tweetid">>, DecodeData)),
                    Tweeter = binary_to_list(maps:get(<<"tweeter">>, DecodeData)),
                    io:fwrite("New Tweet \nTweet\t~p \nTweeter \t~p \nTweetID\t~p\n", [Tweet, Tweeter, TweetId])
            end;

        {user_interact, Action, Data} ->
            case Action of
                "tweet" ->
                    Message = #{<<"Query">> => <<"tweet">>, <<"Username">> => Username, <<"Tweet">> => Data},
                    EncodedMessage = jsone:encode(Message),
                    gun:ws_send(ConnPid, StreamRef, {text, EncodedMessage});
                "query" ->
                    Message = #{<<"Query">> => <<"query">>, <<"Username">> => Username, <<"QueryString">> => Data},
                    EncodedMessage = jsone:encode(Message),
                    gun:ws_send(ConnPid, StreamRef, {text, EncodedMessage});
                "subscribe" ->
                    Message = #{<<"Query">> => <<"subscribe">>, <<"Username">> => Username, <<"SubscribeString">> => Data},
                    EncodedMessage = jsone:encode(Message),
                    gun:ws_send(ConnPid, StreamRef, {text, EncodedMessage});
                "retweet" ->
                    Message = #{<<"Query">> => <<"retweet">>, <<"Username">> => Username, <<"TweetId">> => Data},
                    EncodedMessage = jsone:encode(Message),
                    gun:ws_send(ConnPid, StreamRef, {text, EncodedMessage})
            end
    end,
    start_listening(Username, ConnPid, StreamRef).

user_interact(Pid) ->
    Operation = string:chomp(io:get_line("1.tweet \n2.query \n3.subscribe \n4.retweet\n")),
    case Operation of 
        "1" -> tweet(Pid);
        "2" -> myquery(Pid);
        "3" -> subscribe(Pid);
        "4" -> retweet(Pid);
        _ -> ok
    end,
    user_interact(Pid).

tweet(Pid) ->
    Tweet = list_to_binary(string:chomp(io:get_line("What would you like to say?\n"))),
    Pid ! {user_interact, "tweet", Tweet}.

myquery(Pid)->
    Query_raw = string:chomp(io:get_line("What/Whom do you want to know more about?\n")),
    Query = list_to_binary(process_string(Query_raw)),
    Pid ! {user_interact, "query", Query}.

subscribe(Pid) ->
    New_subscription_raw = string:chomp(io:get_line("What are you subscribing to?\n")),
    New_subscription = list_to_binary(process_string(New_subscription_raw)),
    Pid ! {user_interact, "subscribe", New_subscription}.

retweet(Pid)->
    {ok, TweetId} = io:read("Enter the tweet id you'd like to retweet.\n"),
    Pid ! {user_interact, "retweet", TweetId}.

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