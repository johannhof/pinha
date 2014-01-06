PROJECT = pinha

DEPS = cowboy couchbeam jsx
dep_cowboy = pkg://cowboy master
dep_couchbeam = https://github.com/benoitc/couchbeam.git master
dep_jsx = https://github.com/talentdeficit/jsx.git master

include erlang.mk
