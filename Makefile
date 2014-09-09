PROJECT = dist_wamp_router
ERLC_OPTS = +debug_info

DEPS = cowboy ranch erwa gproc
dep_cowboy = pkg://cowboy 0.10.0
dep_ranch = pkg://ranch 0.10.0
dep_erwa = https://github.com/arnehilmann/erwa.git
dep_gproc = https://github.com/uwiger/gproc.git

include ./erlang.mk
