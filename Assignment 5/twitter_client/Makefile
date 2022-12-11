PROJECT = twitter_client
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = gun jsone

BUILD_DEPS += relx
include erlang.mk
ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))