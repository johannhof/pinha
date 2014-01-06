-module(pinha_player).

-import(pinha_utils, [replace/2, extract/2, formatted_now/0]).

-export([start/1, stop/0]).

unidentified() ->
  receive
    {set_id, [{id, ID}]} ->
      db ! {self(), open_or_create, ID},
      receive
        {known_user, Player} ->
          put(player, Player),
          get(client) ! [known_user | Player];
        {unknown_user, Player} ->
          put(player, Player),
          get(client) ! [unknown_user];
        disconnected ->
          general(disconnected, fun unidentified/0)
      end,
      nowhere();
    disconnected ->
      general(disconnected, fun unidentified/0);
    _ ->
      unidentified()
  end.

nowhere() ->
  receive
    {go_to_lobby, _} ->
      lobby ! {self(), add, get(player)},
      lobby();
    Other ->
      general(Other, fun nowhere/0)
  end.

lobby() ->
  receive
    {leave_lobby, _} ->
      lobby ! {self(), remove},
      nowhere();
    {pair, {partner, PartnerID}} ->
      lobby ! {self(), pair, PartnerID},
      lobby();
    {request, Pid} ->
      erlang:display("Message got through!"),
      erlang:display(Pid);
    Update = [lobby_update | _Data] ->
      get(client) ! Update,
      lobby();
    disconnected ->
      lobby ! {self(), remove},
      general(disconnected, {});
    Other ->
      general(Other, fun lobby/0)
  end.

general({set_name, [{name, Name} | _]}, F) ->
  db ! {self(), save, replace(get(player), {name, Name})},
  receive
    {ok, Player} ->
      put(player, Player),
      get(client) ! [confirm, {name, Name}]
  end,
  F();

general({get_name, _}, F) ->
  get(client) ! [send_name, {name, extract(get(player), name)}],
  F();

% unknown action
general({Action, _}, F) when is_atom(Action) ->
  {name, Name} = erlang:fun_info(F, name),
  log_error(io_lib:format("Client has the status ~s. It can not receive the action ~s", [Name, Action])),
  F();

general(disconnected, _) ->
  stop().

start(Pid) ->
  log_info("A client connected"),
  put(client, Pid),
  unidentified().

stop() ->
  log_info("Client disconnected"),
  exit(normal).

log_error(Message) ->
  io:format("~s [ERROR|CLIENT] ~s.~n", [formatted_now(), Message]).

log_info(Message) ->
  io:format("~s [INFO|CLIENT] ~s.~n", [formatted_now(), Message]).
