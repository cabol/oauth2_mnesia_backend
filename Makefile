PROJECT = oauth2_mnesia_backend

DEPS = oauth2

dep_oauth2 = git https://github.com/kivra/oauth2.git 0.6.0

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk
