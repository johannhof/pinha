-module(pinha_game_service).

-import(pinha_utils, [log/2]).

-export([start/0, stop/0]).

enter_game(Pid, IDs = {ID1, ID2})->
  GameID = cantor(ID1, ID2), % make unique id from player ids
  case ets:lookup(running, GameID) of
    [] -> % no game running
      log("creating a new game"),
      ets:insert(running, {
                   GameID,
                   Game = spawn(pinha_game, start, [new, Pid, IDs])
                  }),
      Pid ! Game;

    [{GameID, Game}] -> %% TODO: enable shutting down of game processes
      log("found a running game"),
      Game ! {Pid, enter, ID1},
      Pid ! Game
  end.

% Implementation of the cantor pairing function
% generates a unique number from two other numbers
cantor(X,Y) -> (math:pow(X, 2) + 3*X + 2*X*Y + Y + math:pow(Y,2)) / 2.

loop() ->
  receive
    {Pid, enter_game, IDs} -> enter_game(Pid, IDs)
  end,
  loop().

start() ->
  dets:open_file(games, [{type, duplicate_bag}, {file, "../games"}]),
  ets:new(running, [named_table, set]),
  loop().

stop() ->
  dets:close(games),
  ok.

log(Message) ->
  log(Message, "GAME SERVICE").
