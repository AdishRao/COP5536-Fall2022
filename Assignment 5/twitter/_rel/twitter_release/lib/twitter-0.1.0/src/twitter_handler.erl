-module(twitter_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).


init(Req, State) ->
    io:format("websocket connection initiated~n~p~n~nstate: ~p~n", [Req, State]),
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
	{ok, State}.


websocket_handle({text, _Data}, State) ->
    DecodeData = jsone:decode(_Data),
	QueryType = binary_to_list(maps:get(<<"Query">>, DecodeData)),
	Username = binary_to_list(maps:get(<<"Username">>, DecodeData)),
	case QueryType of
		"register" ->
			Password = binary_to_list(maps:get(<<"Password">>, DecodeData)),
			Status = register_user(Username, Password),
			Message = #{<<"response">> => Status, <<"username">> => list_to_binary(Username)},
			EncodedMessage = jsone:encode(Message),
			Pid = self(),
			[UserHanlder] = State, 
			UserHanlder ! {register_user, Username, Pid},
    		{[{text, EncodedMessage}], State ++ [Pid]};
		"login" ->
			Password = binary_to_list(maps:get(<<"Password">>, DecodeData)),
			Status = login_user(Username, Password),
			Message = #{<<"response">> => Status, <<"username">> => list_to_binary(Username)},
			EncodedMessage = jsone:encode(Message),
			Pid = self(),
			[UserHanlder] = State,
			UserHanlder ! {register_user, Username, Pid},
    		{[{text, EncodedMessage}], State ++ [Pid]};
		"tweet" ->
			Tweet = binary_to_list(maps:get(<<"Tweet">>, DecodeData)),
			{Status, FinalUsers, TweetId} = client_tweet(Username, Tweet),
			Message = #{<<"response">> => <<"tweet_response">>, <<"status">> => Status},
			EncodedMessage = jsone:encode(Message),
			case Status of
				<<"ok">> ->
					[UserHanlder, _] = State,
					UserHanlder ! {send_tweets, Tweet, FinalUsers, Username, TweetId};
				<<"error">> ->
					ok
			end,
			{[{text, EncodedMessage}], State};
		"query" ->
			QueryString = binary_to_list(maps:get(<<"QueryString">>, DecodeData)),
			[Ret1, Ret2, Ret3] = query_tweets(Username, QueryString),
			{server_query, _, TweetString1, TweetId1, TweeterNameString1} = Ret1,
			{server_query, _, TweetString2, TweetId2, TweeterNameString2} = Ret2,
			{server_query, _, TweetString3, TweetId3, TweeterNameString3} = Ret3,
			if TweetString1 == error ->
				if TweetString2 == error ->
					if TweetString3 == error ->
						{ok, State};
					true ->
						Message = #{<<"response">> => <<"query_response">>, <<"tweet">> => list_to_binary(TweetString3), <<"tweetid">> => TweetId3, <<"tweeter">> => list_to_binary(TweeterNameString3)},
						EncodedMessage = jsone:encode(Message),
						{[{text, EncodedMessage}], State}
					end;
				true -> 
					Message = #{<<"response">> => <<"query_response">>, <<"tweet">> => list_to_binary(TweetString2), <<"tweetid">> => TweetId2, <<"tweeter">> => list_to_binary(TweeterNameString2)},
					EncodedMessage = jsone:encode(Message),
					{[{text, EncodedMessage}], State}
				end;
			true ->
				Message = #{<<"response">> => <<"query_response">>, <<"tweet">> => list_to_binary(TweetString1), <<"tweetid">> => TweetId1, <<"tweeter">> => list_to_binary(TweeterNameString1)},
				EncodedMessage = jsone:encode(Message),
				{[{text, EncodedMessage}], State}
			end;
		"subscribe" ->
			SubscribeString = binary_to_list(maps:get(<<"SubscribeString">>, DecodeData)),
			subscribe_to(Username, SubscribeString),
			{ok, State};
		"retweet" ->
			TweetId = maps:get(<<"TweetId">>, DecodeData),
			{Status, FinalUsers, NewTweetId, Tweet} = retweet(Username, TweetId),
			Message = #{<<"response">> => <<"tweet_response">>, <<"status">> => Status},
			EncodedMessage = jsone:encode(Message),
			case Status of
				<<"ok">> ->
					[UserHanlder, _] = State,
					UserHanlder ! {send_tweets, Tweet, FinalUsers, Username, NewTweetId};
				<<"error">> ->
					ok
			end,
			{[{text, EncodedMessage}], State}
	end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({new_tweet, Tweet, Tweeter, TweetId}, State) ->
	Message = #{<<"response">> => <<"new_tweet">>, <<"tweet">> => list_to_binary(Tweet), <<"tweetid">> => TweetId, <<"tweeter">> => list_to_binary(Tweeter)},
	EncodedMessage = jsone:encode(Message),
	{[{text, EncodedMessage}], State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, Req, _State) ->
    io:format("websocket connection terminated~n~p~n", [maps:get(peer, Req)]),
	[UserHanlder, Pid] = _State,
	UserHanlder ! {remove_user, Pid},
    ok.


register_user(Username, Password) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    {ok, _, Res0} = mysql:query(Pid, "SHOW TABLES LIKE 'Users';"),
    if length(Res0) == 0 ->
        ok = mysql:query(Pid, "CREATE TABLE Users (
            username varchar(255) UNIQUE,
            password varchar(255)
        );");
    true ->
        ok
    end,
    Res1 = mysql:query(Pid, "INSERT INTO Users VALUES (?, ?);", [Username, Password]),
    if Res1 == ok ->
        UsernameSub = string:concat(Username, "_subscribers"),
        UsernameTo = string:concat(Username, "_subscribed_to"),
        UsernameSubQuery = string:concat(string:concat("CREATE TABLE ", UsernameSub), " (username varchar(255) UNIQUE);"),
        UsernameToQuery = string:concat(string:concat("CREATE TABLE ", UsernameTo), " (name varchar(255) UNIQUE, tweet_id INT);"),
        ok = mysql:query(Pid, UsernameSubQuery),
        ok = mysql:query(Pid, UsernameToQuery),
		Status = list_to_binary("ok");
    true ->
        Status = list_to_binary("error")
    end,
    mysql:stop(Pid),
    Status.

login_user(Username, Password) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    {ok, _, Res0} = mysql:query(Pid, "SHOW TABLES LIKE 'Users';"),
    if length(Res0) == 0 ->
        Status = list_to_binary("error");
    true ->
        {ok, _, StoredPasswordList} = mysql:query(Pid, "SELECT password from Users WHERE username = ?;", [Username]),
        if length(StoredPasswordList) == 0 ->
            Status = list_to_binary("error");
        true ->
            StoredPassword = lists:nth(1, lists:nth(1, StoredPasswordList)),
            StringStoredPassword = binary_to_list(StoredPassword),
            if StringStoredPassword == Password ->
                Status = list_to_binary("ok");
            true ->
                Status = list_to_binary("error")
            end
        end
    end,
    mysql:stop(Pid),
    Status.

client_tweet(Username, Tweet) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    {ok, _, Res0} = mysql:query(Pid, "SHOW TABLES LIKE 'Tweets';"),
    if length(Res0) == 0 ->
        ok = mysql:query(Pid, "CREATE TABLE Tweets (
            tweet_id INT AUTO_INCREMENT,
            username varchar(255),
            tweet TEXT,
            primary key (tweet_id)
        );");
    true ->
        ok
    end,
    Res1 = mysql:query(Pid, "INSERT INTO Tweets (username, tweet) VALUES (?, ?);", [Username, Tweet]),
    if Res1 /= ok ->
        Status = list_to_binary("error"),
		Ret = {Status, error, error};
    true ->
        TweetId = mysql:insert_id(Pid),
        Users = process_tweet(Tweet, TweetId, Pid),
        UserSubscribers = string:concat(Username, "_subscribers"),
        SelectQuery = string:concat(string:concat("SELECT * from ", UserSubscribers), ";"),
        {ok, _, Res2} = mysql:query(Pid, SelectQuery),
        FinalUsers = sets:to_list(sets:from_list(Users ++ Res2)),
		Status = list_to_binary("ok"),
		Ret = {Status, FinalUsers, TweetId}
    end,
    mysql:stop(Pid),
    Ret.

process_tweet(Tweet, TweetId, Pid) ->
    ModifiedTweet = string:concat(Tweet," "),
    case re:run(ModifiedTweet, "#[^ ]*", [global]) of
        {match, HashCaptured} ->
            HashUsers = insert_into_hash_table(Tweet, TweetId, HashCaptured, Pid);
        nomatch ->
            HashUsers = []
    end,
    case re:run(ModifiedTweet, "@[^ ]*", [global]) of
        {match, MentionCaptured} ->
            MentionUsers = insert_into_mention_table(Tweet, TweetId, MentionCaptured, Pid);
        nomatch ->
            MentionUsers = []
    end,
    HashUsers ++ MentionUsers.

insert_into_hash_table(_, _, [], _) ->
    [];
insert_into_hash_table(Tweet, TweetId, HashCaptured, Pid) ->
    [CurrentHash | Remainder] = HashCaptured,
    {Start, Length} = lists:nth(1, CurrentHash),
    HashString = string:slice(Tweet, Start, Length),
    StringWithoutHash = string:prefix(HashString, "#"),
    FinalString =  string:concat("hash_", StringWithoutHash),
    FinalStringUsers = string:concat(FinalString, "_users"),
    ShowQuery = string:concat(string:concat("SHOW TABLES LIKE '", FinalString), "';"),
    {ok, _, Res0} = mysql:query(Pid, ShowQuery),
    if length(Res0) == 0 ->
        FinalStringQuery = string:concat(string:concat("CREATE TABLE ", FinalString), " (tweet_id INT);"),
        FinalStringUsersQuery = string:concat(string:concat("CREATE TABLE ", FinalStringUsers), " (username varchar(255) UNIQUE);"),
        ok = mysql:query(Pid, FinalStringQuery),
        ok = mysql:query(Pid, FinalStringUsersQuery);
    true ->
        ok
    end,
    InsertQuery = string:concat(string:concat("INSERT INTO ", FinalString), " VALUES (?);"),
    ok = mysql:query(Pid, InsertQuery, [TweetId]),
    SelectQuery = string:concat(string:concat("SELECT * from ", FinalStringUsers), ";"),
    case mysql:query(Pid, SelectQuery) of
        {ok, _, Res2} ->
            ok;
        {error, _} ->
            Res2 = []
    end,
    Res2 ++ insert_into_hash_table(Tweet, TweetId, Remainder, Pid).

insert_into_mention_table(_, _, [], _) ->
    [];
insert_into_mention_table(Tweet, TweetId, MentionCaptured, Pid) ->
    [CurrentMention | Remainder] = MentionCaptured,
    {Start, Length} = lists:nth(1, CurrentMention),
    MentionString = string:slice(Tweet, Start, Length),
    StringWithoutMention = string:prefix(MentionString, "@"),
    FinalString =  string:concat("mention_", StringWithoutMention),
    FinalStringUsers = string:concat(FinalString, "_users"),
    ShowQuery = string:concat(string:concat("SHOW TABLES LIKE '", FinalString), "';"),
    {ok, _, Res0} = mysql:query(Pid, ShowQuery),
    if length(Res0) == 0 ->
        FinalStringQuery = string:concat(string:concat("CREATE TABLE ", FinalString), " (tweet_id INT);"),
        FinalStringUsersQuery = string:concat(string:concat("CREATE TABLE ", FinalStringUsers), " (username varchar(255) UNIQUE);"),
        ok = mysql:query(Pid, FinalStringQuery),
        ok = mysql:query(Pid, FinalStringUsersQuery);
    true ->
        ok
    end,
    InsertQuery = string:concat(string:concat("INSERT INTO ", FinalString), " VALUES (?);"),
    ok = mysql:query(Pid, InsertQuery, [TweetId]),
    SelectQuery = string:concat(string:concat("SELECT * from ", FinalStringUsers), ";"),
    case mysql:query(Pid, SelectQuery) of
        {ok, _, Res2} ->
            ok;
        {error, _} ->
            Res2 = []
    end,
    Res2 ++ insert_into_mention_table(Tweet, TweetId, Remainder, Pid).

query_tweets(Username, Name) ->
	io:fwrite("Query ~p\n\n", [Name]),
	{ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
	case string:prefix(Name, "hash_") of
		nomatch ->
			Ret1 = {server_query, error, error, error, error},
			MatchHash = false;
		_ ->
			ShowQuery = string:concat(string:concat("SHOW TABLES LIKE '", Name), "';"),
			{ok, _, Res0} = mysql:query(Pid, ShowQuery),
			if length(Res0) == 0 ->
				Ret1 = {server_query, error, error, error, error};
			true ->
				SelectQuery = string:concat(string:concat("SELECT * FROM ", Name), " ORDER BY tweet_id DESC LIMIT 1;"),
				{ok, _, SelectRes} = mysql:query(Pid, SelectQuery),
				if SelectRes == [] ->
					Ret1 = {server_query, error, error, error, error};
				true ->
					TweetId = lists:nth(1, lists:nth(1, SelectRes)),
					{ok, _, SelectRes1} = mysql:query(Pid, "SELECT username, tweet FROM Tweets WHERE tweet_id = ?",[TweetId]),
					[TweeterName, Tweet] = lists:nth(1, SelectRes1),
					TweeterNameString = binary_to_list(TweeterName),
					TweetString = binary_to_list(Tweet),
					Ret1 = {server_query, Username, TweetString, TweetId, TweeterNameString}
				end
			end,
			MatchHash = true
	end,
	case string:prefix(Name, "mention_") of
		nomatch ->
			Ret2 = {server_query, error, error, error, error},
			MatchMention = false;
		_ ->
			ShowQuery1 = string:concat(string:concat("SHOW TABLES LIKE '", Name), "';"),
			{ok, _, Res1} = mysql:query(Pid, ShowQuery1),
			if length(Res1) == 0 ->
				Ret2 = {server_query, error, error, error, error};
			true ->
				SelectQuery1 = string:concat(string:concat("SELECT * FROM ", Name), " ORDER BY tweet_id DESC LIMIT 1;"),
				{ok, _, SelectRes2} = mysql:query(Pid, SelectQuery1),
				if SelectRes2 == [] ->
					Ret2 = {server_query, error, error, error, error};
				true ->
					TweetId1 = lists:nth(1, lists:nth(1, SelectRes2)),
					{ok, _, SelectRes3} = mysql:query(Pid, "SELECT username, tweet FROM Tweets WHERE tweet_id = ?",[TweetId1]),
					[TweeterName1, Tweet1] = lists:nth(1, SelectRes3),
					TweeterNameString1 = binary_to_list(TweeterName1),
					TweetString1 = binary_to_list(Tweet1),
					Ret2 = {server_query, Username, TweetString1, TweetId1, TweeterNameString1}
				end
			end,
			MatchMention = true
	end,
	if (MatchMention == false) and (MatchHash == false) ->
		SelectQuery2 = "SELECT tweet, tweet_id FROM Tweets WHERE username = (?) ORDER BY tweet_id DESC LIMIT 1",
		{ok, _, SelectRes4} = mysql:query(Pid, SelectQuery2, [Name]),
		if SelectRes4 == [] ->
			Ret3 = {server_query, error, error, error, error};
		true ->
			[Tweet2, TweetId2] = lists:nth(1, SelectRes4),
			TweetString2 = binary_to_list(Tweet2),
			Ret3 = {server_query, Username, TweetString2, TweetId2, Name}
		end;
	true ->
		Ret3 = {server_query, error, error, error, error}
	end,
	mysql:stop(Pid),
	[Ret1, Ret2, Ret3].

subscribe_to(Username, Name) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    case string:prefix(Name, "hash_") of
        nomatch ->
            MatchHash = false;
        _ ->
            ShowQuery = string:concat(string:concat("SHOW TABLES LIKE '", Name), "';"),
            {ok, _, Res0} = mysql:query(Pid, ShowQuery),
            if length(Res0) == 0 ->
                NameQuery = string:concat(string:concat("CREATE TABLE ", Name), " (tweet_id INT);"),
                NameUsersQuery = string:concat(string:concat("CREATE TABLE ", string:concat(Name, "_users")), " (username varchar(255) UNIQUE);"),
                ok = mysql:query(Pid, NameQuery),
                ok = mysql:query(Pid, NameUsersQuery);
            true ->
                ok
            end,
            InsertQuery = string:concat(string:concat("INSERT IGNORE INTO ", string:concat(Name, "_users")), " VALUES(?)"),
            ok = mysql:query(Pid, InsertQuery, [Username]),
            MatchHash = true
    end,
    case string:prefix(Name, "mention_") of
        nomatch ->
            MatchMention = false;
        _ ->
            ShowQuery1 = string:concat(string:concat("SHOW TABLES LIKE '", Name), "';"),
            {ok, _, Res1} = mysql:query(Pid, ShowQuery1),
            if length(Res1) == 0 ->
                NameQuery1 = string:concat(string:concat("CREATE TABLE ", Name), " (tweet_id INT);"),
                NameUsersQuery1 = string:concat(string:concat("CREATE TABLE ", string:concat(Name, "_users")), " (username varchar(255) UNIQUE);"),
                ok = mysql:query(Pid, NameQuery1),
                ok = mysql:query(Pid, NameUsersQuery1);
            true ->
                ok
            end,
            InsertQuery1 = string:concat(string:concat("INSERT IGNORE INTO ", string:concat(Name, "_users")), " VALUES(?)"),
            ok = mysql:query(Pid, InsertQuery1, [Username]),
            MatchMention = true
    end,
    if (MatchMention == false) and (MatchHash == false) ->
        ShowQuery2 = string:concat(string:concat("SHOW TABLES LIKE '", string:concat(Name, "_subscribers")), "';"),
        {ok, _, Res2} = mysql:query(Pid, ShowQuery2),
        if length(Res2) == 0 ->
            ok;
        true ->
            InsertQuery2 = string:concat(string:concat("INSERT IGNORE INTO ",string:concat(Name, "_subscribers")), " VALUES(?)"),
            ok = mysql:query(Pid, InsertQuery2, [Username])
        end;
    true ->
        ok
    end,
    mysql:stop(Pid).

retweet(Username, TweetId) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    SelectQuery = "SELECT username, tweet FROM Tweets WHERE tweet_id = (?)",
    {ok, _, SelectRes} = mysql:query(Pid, SelectQuery, [TweetId]),
    mysql:stop(Pid),
    if SelectRes == [] ->
		Status = list_to_binary("error"),
        Ret = {Status, error, error, error};
    true ->
        [Tweeter_raw, Tweet_raw] = lists:nth(1, SelectRes),
        Tweet = binary_to_list(Tweet_raw),
        Tweeter = binary_to_list(Tweeter_raw),
        NewTweet = string:concat(string:concat(Tweeter, " said : \""), string:concat(Tweet, "\"")),
        {Status, FinalUsers, NewTweetId} = client_tweet(Username, NewTweet),
		Ret = {Status, FinalUsers, NewTweetId, NewTweet}
    end,
	Ret.