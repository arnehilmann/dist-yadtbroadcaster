PROJECT = dist_wamp_router
ERLC_OPTS = +debug_info

DEPS = cowboy ranch erwa hackney
dep_cowboy = pkg://cowboy 0.10.0
dep_ranch = pkg://ranch 0.10.0
dep_erwa = https://github.com/arnehilmann/erwa.git
dep_hackney = https://github.com/benoitc/hackney.git
#dep_gproc = https://github.com/uwiger/gproc.git
#dep_rhc = https://github.com/basho/riak-erlang-http-client.git

include ./erlang.mk
