PROJECT = twitter
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsone mysql
dep_cowboy_commit = 2.9.0
dep_mysql_commit = 1.7.0

DEP_PLUGINS = cowboy

REL_DEPS = relx

include erlang.mk
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))