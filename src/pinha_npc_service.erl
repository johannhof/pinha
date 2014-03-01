-module(pinha_npc_service).

-export([start/0, stop/0, get_random/1, insert_if_suited/1]).

start() ->
  dets:open_file(npcs, [{type, set}, {file, "../npcs"}]).

stop() ->
  dets:close(npcs).

get_random(Game) ->
  spawn(pinha_npc, start, [Game, get_random_gamedata()]).

get_random_gamedata() ->
  Size = dets:info(npcs, size),
  [{_, Data} | _] = dets:lookup(npcs, random:uniform(Size - 1)),
  Data.

insert_if_suited(_Winner = {player, _Pid, Id, _Name, Left, _Turn, Turns}) ->
  case Id of
    1 -> % is already an npc
      ok;
    _ ->
      case Left of
        true -> % we only want lefties
          check_turn_number(Turns);
        false ->
          ok
      end
  end.

check_turn_number(Turns) ->
  Length = lists:flatlength(Turns),
  if
    Length > 3 -> insert(Turns);
    true -> ok
  end.

insert(Turns) ->
  Size = dets:info(npcs, size),
  dets:insert(npcs, {Size, Turns}).
