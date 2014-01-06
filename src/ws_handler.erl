-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-import(jsx, [decode/1, encode/1]).
-import(pinha_utils, [atomize/1]).
-import(pinha_action, [client_action/1, server_action/1]).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  put(player, spawn(pinha_player, start, [self()])),
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  case catch decode(Msg) of
    {'EXIT', _Why} ->
      io:format("[ERROR|CLIENT] Error decoding json message~n");

    K when is_list(K) ->
      case catch atomize(K) of
        [{action, Action} | Data] ->
          get(player) ! {client_action(Action), Data};
        _ ->
          io:format("[WARNING|CLIENT] received message without an action~n")
      end;

    _ ->
      io:format("[WARNING|CLIENT] received weird message~n")

  end,
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info([Action | Data], Req, State) ->
  Enc = encode([{action, server_action(Action)} | Data]),
  {reply, {text, Enc}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  get(player) ! disconnected,
  ok.
