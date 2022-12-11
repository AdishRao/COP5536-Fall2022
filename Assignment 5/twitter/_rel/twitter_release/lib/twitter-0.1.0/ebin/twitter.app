{application, 'twitter', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['twitter_app','twitter_handler','twitter_sup']},
	{registered, [twitter_sup]},
	{applications, [kernel,stdlib,cowboy,jsone,mysql]},
	{mod, {twitter_app, []}},
	{env, []}
]}.