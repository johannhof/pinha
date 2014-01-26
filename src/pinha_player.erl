-module(pinha_player).

-import(pinha_utils, [replace/2, extract/2, log/2, log_error/2, except/2]).

-import(io_lib, [format/2]).

-export([start/1, stop/0]).

% Process memory: TODO get rid of this
% client - Pid of the handler that sends messages to the client
% player - database object of the player
% name - name of the player
% lobby - is the player in the lobby right now? (ugly)

start(Pid) ->
  log("A client connected"),
  put(client, Pid),
  unidentified().

stop() ->
  log(format("~s has disconnected", [get(name)])),
  exit(normal).

leave_lobby() ->
  lobby ! {self(), remove},
  put(lobby, false). % TODO ugly hack to set lobby state,
  % better check in lobby if player already exists (set instead of list?)

unidentified() ->
  receive
    {set_id, [{id, ID}]} ->
      db ! {self(), open_or_create, ID},
      receive
        {known_user, Player} ->
          put(player, Player),
          put(name, extract(Player, name)),
          get(client) ! [known_user | Player];
        {unknown_user, Player} ->
          put(player, Player),
          put(name, extract(Player, name)),
          get(client) ! [unknown_user]
      end,
      main();
    disconnected -> stop()
  end.

main() ->
  receive
    {go_to_lobby, _} ->
      Lobby = get(lobby),
      if Lobby /= true ->
           lobby ! {self(), add, except(['_id', '_rev', games], get(player))},
           put(lobby, true);
         true -> erlang:display(Lobby)
      end,
      main();

    {leave_lobby, _} ->
      leave_lobby(),
      main();

    % create a new game by challenging the specified partner
    {enter_game, [{partner, PartnerID} | _]} ->
      leave_lobby(),
      log(format("~s wants to play a game")),
      game_service ! {self(), enter_game, {extract(get(player), public_id), PartnerID}},
      receive Game when is_pid(Game) -> game(Game) end; %% TODO: save game id in player

    {set_name, [{name, Name} | _]} -> set_name(Name);
    {get_name, _} -> get_name();

    disconnected ->
      Lobby = get(lobby),
      if Lobby =:= true ->
           lobby ! {self(), remove},
           put(lobby, false) % TODO ugly hack to set lobby state,
      end,
      stop();

    % forward lobby update to client
    Update = [lobby_update | _Data] ->
      Lobby = get(lobby),
      if Lobby =:= true ->
           get(client) ! Update
      end,
      main()
  end.

leave_game(Game) ->
  log(format("~s has left a game.")),
  Game ! {self(), leave}.

game(Game) ->
  receive
    % Player messages

    {end_turn, Data} ->
      log(format("~s has ended his turn")),
      Game ! {self(), end_turn, Data},
      game(Game);

    {lose, _} ->
      log(format("~s has declared that he lost")),
      Game ! {self(), lose};

    {leave_game, _} ->
      leave_game(Game),
      main();

    disconnected ->
      leave_game(Game),
      stop();

    % Game messages

    {Game, turn, Data} ->
      get(player) ! [turn, Data],
      game(Game);

    {Game, world, Data} ->
      get(player) ! [world, Data],
      game(Game);

    {Game, you_win}-> win();
    {Game, you_lose}-> lose();
    {Game, draw}-> draw()
  end.

%% ENDGAME BEHAVIOR %%

draw() ->
  get(client) ! [draw],
  main().

lose() ->
  Losses = extract(get(player), lost),
  db ! {self(), save, replace(get(player), {lost, Losses + 1})},
  receive
    {ok, Player} ->
      put(player, Player)
  end,
  main().

win() ->
  log(format("~s has won the game. Yeah!")),
  get(client) ! [you_win],
  Wins = extract(get(player), won),
  db ! {self(), save, replace(get(player), {won, Wins + 1})},
  receive
    {ok, Player} ->
      put(player, Player)
  end,
  main().

%% HELPERS %%

get_name() ->
  log(format("~s has asked for his name")),
  get(client) ! [send_name, {name, get(name)}],
  main().

set_name(Name) ->
  log(format("~s has set his name to ~s", [get(name), Name])),
  db ! {self(), save, replace(get(player), {name, Name})},
  receive
    {ok, Player} ->
      put(name, Name),
      put(player, Player),
      get(client) ! [confirm, {name, Name}]
  end,
  main().

format(Message) ->
  format(Message, [get(name)]).

log(Message) ->
  log(Message, "PLAYER").

%log_error(Message) ->
%log_error(Message, "PLAYER").
