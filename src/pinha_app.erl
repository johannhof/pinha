%% @private
-module(pinha_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  register(db, spawn(pinha_db, start, [])),
  register(lobby, spawn(pinha_lobby, start, [])),
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/", ws_handler, []}
                                          ]}
                                   ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]),
  websocket_sup:start_link().

stop(_State) ->
  ok.
