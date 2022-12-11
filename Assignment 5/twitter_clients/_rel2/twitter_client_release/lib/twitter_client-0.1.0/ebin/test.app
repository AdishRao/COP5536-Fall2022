{application, 'test', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['my_client','twitter_client_app','twitter_client_sup']},
	{registered, []},
	{applications, [kernel,stdlib,jsone]},
	{env, []}
]}.