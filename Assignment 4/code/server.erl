-module(server).

% ignore
-compile(export_all).

start() ->
    register(server, self()),
    code:add_pathz("../rebar3/_build/default/lib/mysql/ebin"),
    LoggedIn = dict:new(),
    start(LoggedIn, []).

start(LoggedIn, Threads) ->
    receive
        % A server spawn has finished work, reduce number of allocated threads.
        {'DOWN', _, process, Pid2, thread_complete} ->
            NewThreads = lists:delete(Pid2, Threads),
            start(LoggedIn, NewThreads);

        % TODO: Handle user logged out
        {'DOWN', _, process, Pid2, _} ->
            Username = get(Pid2),
            erase(Pid2),
            NewLoggedIn = dict:erase(Username, LoggedIn),
            start(NewLoggedIn, Threads);

        % Client Request To Register a user
        {client_register, ClientPid, Username, Password} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, register_user, [ClientPid, Username, Password]),
            monitor(process, SpawnThread),
            start(LoggedIn, NewThreads ++ [SpawnThread]);

        % Server thread response to register request
        {server_register, Status, Username, ClientPid} ->
            if Status == error ->
                ClientPid ! {register_response, error},
                NewLoggedIn = LoggedIn;
            true ->
                ClientPid ! {register_response, Username, ok},
                monitor(process, ClientPid),
                NewLoggedIn = dict:store(Username, ClientPid, LoggedIn),
                put(ClientPid, Username)
            end,
            start(NewLoggedIn, Threads);

        % Client Login Request
        {client_login, ClientPid, Username, Password} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, login_user, [ClientPid, Username, Password]),
            monitor(process, SpawnThread),
            start(LoggedIn, NewThreads ++ [SpawnThread]);

        % Server thread response to Login Request
        % TODO: Search DB for all following and last tweet recieved.
        {server_login, Status, Username, ClientPid} ->
            if Status == error ->
                ClientPid ! {login_response, error},
                NewLoggedIn = LoggedIn;
            true ->
                ClientPid ! {login_response, Username, ok},
                monitor(process, ClientPid),
                NewLoggedIn = dict:store(Username, ClientPid, LoggedIn),
                put(ClientPid, Username)
            end,
            start(NewLoggedIn, Threads);


        % Client Tweet Request
        {client_tweet, Username, Tweet} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, client_tweet, [Username, Tweet]),
            monitor(process, SpawnThread),
            start(LoggedIn, NewThreads ++ [SpawnThread]);

        % Server response to Tweet Request
        {server_tweet, Status, Username, FinalUsers, Tweet, TweetId} ->
            case dict:find(Username, LoggedIn) of
                {ok, ClientPid} ->
                    if Status == error ->
                        ClientPid ! {tweet_response, error};
                    true ->
                        ClientPid ! {tweet_response, ok}
                    end;
                error ->
                    ok
            end,
            spawn(server, send_tweets, [Tweet, FinalUsers, LoggedIn, Username, TweetId]),
            start(LoggedIn, Threads);

        % Client wants to subscribe to something
        {client_subscribe_to, Username, Name} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, subscribe_to, [Username, Name]),
            monitor(process, SpawnThread),
            start(LoggedIn, NewThreads ++ [SpawnThread]);

        % Client wants to query to something
        {client_query, Username, Name} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, query_tweets, [Username, Name]),
            monitor(process, SpawnThread),
            start(LoggedIn, NewThreads ++ [SpawnThread]);

        % Server response to client query to something
        {server_query, Username, Tweet, TweetId, Tweeter} ->
            case dict:find(Username, LoggedIn) of
                {ok, ClientPid} ->
                        ClientPid ! {query_response, Tweet, TweetId, Tweeter};
                error ->
                    ok  
            end,
            start(LoggedIn, Threads);

        % Client wants to retweet. Response in server_tweet
        {client_retweet, Username, Tweet, Tweeter} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, retweet, [Username, Tweet, Tweeter]),
            monitor(process, SpawnThread),
            start(LoggedIn, NewThreads ++ [SpawnThread])
    end.

% Server is waiting for a thread to finish
server_wait() ->
    receive
        {'DOWN', _, process, Pid2, thread_complete} ->
            Pid2
    end.

register_user(ClientPid, Username, Password) ->
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
        server ! {server_register, ok, Username, ClientPid},
        UsernameSub = string:concat(Username, "_subscribers"),
        UsernameTo = string:concat(Username, "_subscribed_to"),
        UsernameSubQuery = string:concat(string:concat("CREATE TABLE ", UsernameSub), " (username varchar(255) UNIQUE);"),
        UsernameToQuery = string:concat(string:concat("CREATE TABLE ", UsernameTo), " (name varchar(255) UNIQUE, tweet_id INT);"),
        ok = mysql:query(Pid, UsernameSubQuery),
        ok = mysql:query(Pid, UsernameToQuery);
    true ->
        server ! {server_register, error, Username, ClientPid}
    end,
    mysql:stop(Pid),
    exit(thread_complete).

login_user(ClientPid, Username, Password) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    {ok, _, Res0} = mysql:query(Pid, "SHOW TABLES LIKE 'Users';"),
    if length(Res0) == 0 ->
        server ! {server_login, error, Username, ClientPid};
    true ->
        ok
    end,
    {ok, _, StoredPasswordList} = mysql:query(Pid, "SELECT password from Users WHERE username = ?;", [Username]),
    if length(StoredPasswordList) == 0 ->
        server ! {server_login, error, Username, ClientPid};
    true ->
        StoredPassword = lists:nth(1, lists:nth(1, StoredPasswordList)),
        StringStoredPassword = binary_to_list(StoredPassword),
        if StringStoredPassword == Password ->
            server ! {server_login, ok, Username, ClientPid};
        true ->
            server ! {server_login, error, Username, ClientPid}
        end
    end,
    mysql:stop(Pid),
    exit(thread_complete).

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
        server ! {server_tweet, error, Username, error, error, error};
    true ->
        TweetId = mysql:insert_id(Pid),
        Users = process_tweet(Tweet, TweetId, Pid),
        UserSubscribers = string:concat(Username, "_subscribers"),
        SelectQuery = string:concat(string:concat("SELECT * from ", UserSubscribers), ";"),
        {ok, _, Res2} = mysql:query(Pid, SelectQuery),
        FinalUsers = sets:to_list(sets:from_list(Users ++ Res2)),
        server ! {server_tweet, ok, Username, FinalUsers, Tweet, TweetId}
    end,
    mysql:stop(Pid),
    exit(thread_complete).

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

send_tweets(_, [], _, _, _) ->
    ok;
send_tweets(Tweet, FinalUsers, LoggedIn, Username, TweetId) ->
    [User | Remainder] = FinalUsers,
    UserString = binary_to_list(lists:nth(1, User)),
    case dict:find(UserString, LoggedIn) of
        {ok, ClientPid} ->
            ClientPid ! {new_tweet, Tweet, Username, TweetId}; % Username is the user who made the tweet
        error ->
            ok
    end,
    send_tweets(Tweet, Remainder, LoggedIn, Username, TweetId).

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
    mysql:stop(Pid),
    exit(thread_complete).

query_tweets(Username, Name) ->
    {ok, Pid} = mysql:start_link([{host, "localhost"}, {user, "root"}, {database, "twitter"}]),
    case string:prefix(Name, "hash_") of
        nomatch ->
            MatchHash = false;
        _ ->
            ShowQuery = string:concat(string:concat("SHOW TABLES LIKE '", Name), "';"),
            {ok, _, Res0} = mysql:query(Pid, ShowQuery),
            if length(Res0) == 0 ->
                ok;
            true ->
                SelectQuery = string:concat(string:concat("SELECT * FROM ", Name), " ORDER BY tweet_id DESC LIMIT 1;"),
                {ok, _, SelectRes} = mysql:query(Pid, SelectQuery),
                if SelectRes == [] ->
                    ok;
                true ->
                    TweetId = lists:nth(1, lists:nth(1, SelectRes)),
                    {ok, _, SelectRes1} = mysql:query(Pid, "SELECT username, tweet FROM Tweets WHERE tweet_id = ?",[TweetId]),
                    {TweeterName, Tweet} = lists:nth(1, lists:nth(1, SelectRes1)),
                    TweeterNameString = binary_to_list(TweeterName),
                    TweetString = binary_to_list(Tweet),
                    server ! {server_query, Username, TweetString, TweetId, TweeterNameString}
                end
            end,
            MatchHash = true
    end,
    case string:prefix(Name, "mention_") of
        nomatch ->
            MatchMention = false;
        _ ->
            ShowQuery1 = string:concat(string:concat("SHOW TABLES LIKE '", Name), "';"),
            {ok, _, Res1} = mysql:query(Pid, ShowQuery1),
            if length(Res1) == 0 ->
                ok;
            true ->
                SelectQuery1 = string:concat(string:concat("SELECT * FROM ", Name), " ORDER BY tweet_id DESC LIMIT 1;"),
                {ok, _, SelectRes2} = mysql:query(Pid, SelectQuery1),
                if SelectRes2 == [] ->
                    ok;
                true ->
                    TweetId1 = lists:nth(1, lists:nth(1, SelectRes2)),
                    {ok, _, SelectRes3} = mysql:query(Pid, "SELECT username, tweet FROM Tweets WHERE tweet_id = ?",[TweetId1]),
                    {TweeterName1, Tweet1} = lists:nth(1, lists:nth(1, SelectRes3)),
                    TweeterNameString1 = binary_to_list(TweeterName1),
                    TweetString1 = binary_to_list(Tweet1),
                    server ! {server_query, Username, TweetString1, TweetId1, TweeterNameString1}
                end
            end,
            MatchMention = true
    end,
    if (MatchMention == false) and (MatchHash == false) ->
        SelectQuery2 = "SELECT (tweet, tweet_id) FROM Tweets WHERE username = (?) ORDER BY tweet_id DESC LIMIT 1",
        {ok, _, SelectRes4} = mysql:query(Pid, SelectQuery2, [Name]),
        if SelectRes4 == [] ->
                    ok;
        true ->
            {Tweet2, TweetId2} = lists:nth(1, lists:nth(1, SelectRes4)),
            TweetString2 = binary_to_list(Tweet2),
            server ! {server_query, Username, TweetString2, TweetId2, Name}
        end;
    true ->
        ok
    end,
    mysql:stop(Pid),
    exit(thread_complete).

retweet(Username, Tweet, Tweeter) ->
    NewTweet = string:concat(string:concat(Tweeter, "said : \""), string:concat(Tweet, "\"")),
    client_tweet(Username, NewTweet).
