-module(pinha_npc).

-import(pinha_utils, [log/2]).

-export([start/2, stop/0]).

start(Game, GameData) ->
  log("New npc started"),
  game(Game, 1, GameData).

stop() -> exit(normal).

game(Game, Turn, GameData) when is_pid(Game) ->
  receive

    % Game messages
    {Game, T, turn, _Data} when T =< Turn  ->
      log("received turn data"),
      erlang:display(T),
      {T, Data} = lists:keyfind(T, 1, GameData),
      Game ! {self(), end_turn, Data},
      game(Game, T + 1, GameData);

    {Game, _, you_win, _}-> stop();

    {Game, _, draw, _}-> stop()

  after 5000 ->
          game(Game, Turn, GameData)
  end.

log(Message) ->
  log(Message, "NPC").
