-module(pinha_lobby).

-import(pinha_utils, [extract/2, log/2]).

-export([start/0, stop/0]).

%% TODO find dead processes and throw them out of the lobby

listen(Lobby) ->
  send_update(Lobby),
  receive
    {Pid, add, Data} ->
      log("A client joins the lobby"),
      listen([{Pid, Data, extract(Data, '_id')} | Lobby]);
    {Pid, remove} ->
      log("A client leaves the lobby"),
      listen([{C, P, ID} || {C, P, ID} <- Lobby, C /= Pid]);
    {Pid, pair, PartnerID, Name} ->
      log("A client wants to pair"),
      case findById(Lobby, PartnerID) of
          [{C, _, _}] -> C ! {request, Pid, Name};
          _ -> Pid ! not_found
      end,
      listen(Lobby);
    send_update -> listen(Lobby)
  end.

findById(Lobby, PlayerID) ->
  [Data || Data = {_C, _P, ID} <- Lobby, ID =:= PlayerID].

send_update(Lobby) ->
  send_update([], [C || {C, _, _} <- Lobby], [], [P || {_, P, _} <- Lobby]).

send_update(_, [], _, []) -> ok;
send_update(Clients, [C | Clients2], Players, [P | Players2]) ->
  C ! [lobby_update, {names, Players ++ Players2}],
  send_update([C | Clients], Clients2, [P | Players], Players2).

start() ->
  listen([]),
  ok.

stop() -> ok.

log(Message) -> log(Message, "LOBBY").
