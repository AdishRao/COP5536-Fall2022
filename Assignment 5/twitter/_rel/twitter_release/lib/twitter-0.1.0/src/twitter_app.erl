-module(twitter_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([handle_users/1]).
-export([send_tweets/5]).

%% API.
start(_Type, _Args) ->
	State = [dict:new(), dict:new()],
	Pid = spawn(twitter_app, handle_users, [State]),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/ws", twitter_handler, [Pid]}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	twitter_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).


handle_users(State) ->
	receive
		{register_user, Username, Pid} ->
			UsersPid = lists:nth(1, State),
			PidUsers = lists:nth(2, State),
			NewUsersPid = dict:store(Username, Pid, UsersPid),
			NewPidUsers = dict:store(Pid, Username, PidUsers),
			handle_users([NewUsersPid, NewPidUsers]);
		{remove_user, Pid} ->
			UsersPid = lists:nth(1, State),
			PidUsers = lists:nth(2, State),
			Username = dict:find(Pid, PidUsers),
			NewUsersPid = dict:erase(Username, UsersPid),
			NewPidUsers = dict:erase(Pid, PidUsers),
			handle_users([NewUsersPid, NewPidUsers]);
		{send_tweets, Tweet, FinalUsers, Username, TweetId} ->
			spawn(twitter_app, send_tweets, [Tweet, FinalUsers, lists:nth(1, State), Username, TweetId]),
			handle_users(State)
	end.


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