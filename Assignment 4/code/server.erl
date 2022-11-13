-module(server).

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

        % Client Request To Register a user
        {client_register, ClientPid, Username, Password} ->
            if (length(Threads) == 10) ->
                Pid2 = server_wait(),
                NewThreads = lists:delete(Pid2, Threads);
            true ->
                NewThreads = Threads
            end,
            SpawnThread = spawn(server, register_user, [ClientPid, Username, Password]),
            start(LoggedIn, NewThreads ++ [SpawnThread]);

        % Server thread response to register request
        {server_register, Status, Username, ClientPid} ->
            if Status == error ->
                ClientPid ! {register_response, error},
                NewLoggedIn = LoggedIn;
            true ->
                ClientPid ! {register_response, ok},
                NewLoggedIn = dict:store(Username, ClientPid, LoggedIn)
            end,
            start(NewLoggedIn, Threads)
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
        server ! {server_register, ok, Username, ClientPid};
    true ->
        server ! {server_register, error, Username, ClientPid}
    end,
    exit(thread_complete).

