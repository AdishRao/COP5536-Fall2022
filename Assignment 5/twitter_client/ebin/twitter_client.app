{application, 'twitter_client', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['my_client','twitter_client_app','twitter_client_sup']},
	{registered, [twitter_client_sup]},
	{applications, [kernel,stdlib,gun,jsone]},
	{mod, {twitter_client_app, []}},
	{env, []}
]}.