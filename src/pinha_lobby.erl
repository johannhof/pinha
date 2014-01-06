-module(pinha_lobby).

-import(pinha_utils, [extract/2, formatted_now/0]).

-export([start/0, stop/0]).

listen(Lobby) ->
  send_update(Lobby),
  receive
    {Pid, add, Data} ->
      log_info("A client joins the lobby"),
      listen([{Pid, Data, extract(Data, '_id')} | Lobby]);
    {Pid, remove} ->
      log_info("A client leaves the lobby"),
      listen([{C, P, ID} || {C, P, ID} <- Lobby, C /= Pid]);
    {Pid, pair, PartnerID} ->
      log_info("A client wants to pair"),
      {C, _P, _ID} = findById(Lobby, PartnerID),
      C ! {request, Pid},
      listen(Lobby);
    send_update ->
      listen(Lobby)
  end.

findById(Lobby, PlayerID) ->
  [Player] = [Data || Data = {_C, _P, ID} <- Lobby, ID =:= PlayerID],
  Player.

send_update(Lobby) ->
  send_update([], [C || {C, _, _} <- Lobby], [], [P || {_, P, _} <- Lobby]).

send_update(_, [], _, []) -> ok;
send_update(Clients, [C | Clients2], Players, [P | Players2]) ->
  C ! [lobby_update, {data, Players ++ Players2}],
  send_update([C | Clients], Clients2, [P | Players], Players2).

start() ->
  listen([]),
  ok.

stop() ->
  ok.

log_info(Message) ->
  io:format("~s [INFO|LOBBY] ~s.~n", [formatted_now(), Message]).
