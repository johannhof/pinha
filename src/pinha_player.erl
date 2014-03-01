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
    {set_id, [{id, ID}, {registration_id, RID}]} ->
      log("A client has set his id"),
      db ! {self(), open_or_create, ID, RID},
      receive
        {known_user, Player} ->
          set_rid(Player, RID),
          put(name, extract(Player, name)),
          get(client) ! [known_user | except([games], Player)],
          log(format("found the user ~s"));
        {unknown_user, Player} ->
          put(player, Player),
          put(name, extract(Player, name)),
          get(client) ! [unknown_user],
          log(format("unknown user ~s"))
      end,
      main();
    disconnected -> stop()
  end.

main() ->
  receive
    {go_to_lobby, _} ->
      Lobby = get(lobby),
      if Lobby /= true ->
           lobby ! {self(), add, except(['_id', '_rev', phone_id, games, registration_id], get(player))},
           put(lobby, true);
         true -> ok
      end,
      main();

    {leave_lobby, _} ->
      leave_lobby(),
      main();

    {find_friends, [{friends, Friends} | _]} ->
      log(format("~s wants to find his friends")),
      db ! {self(), find_friends, jsx:decode(Friends)},
      receive
        F = {friends, _} ->
          get(client) ! [friends, F],
          main()
      after 5000 -> main()
      end;

    {facebook_id, [{id, ID} | _]} -> set_facebook_id(ID);

    % create a new game by challenging the specified partner
    {enter_game, [{partner, PartnerID} | _]} -> enter_game(PartnerID);

    {set_name, [{name, Name} | _]} -> set_name(Name);
    {get_name, _} -> get_name();
    {get_running_games, _} -> get_running_games();

    disconnected ->
      Lobby = get(lobby),
      if Lobby =:= true ->
           lobby ! {self(), remove},
           put(lobby, false); % TODO ugly hack to set lobby state,
         true -> ok
      end,
      stop();

    % forward lobby update to client
    Update = [lobby_update | _Data] ->
      Lobby = get(lobby),
      if Lobby =:= true ->
           get(client) ! Update;
         true -> ok
      end,
      main()
  end.

enter_game(PartnerID) ->
  leave_lobby(),
  log(format("~s wants to play a game")),
  game_service ! {self(), enter_game, {extract(get(player), public_id), PartnerID}},
  receive
    {Game, _, new_game, {GameData, Data}} ->
      get(client) ! [new_game | Data],
      add_game(GameData),
      game(Game, 0, GameData);
    {Game, Turn, existing_game, {GameData, Data}} ->
      get(client) ! [existing_game | Data],
      add_game(GameData),
      game(Game, Turn - 1, GameData)
  end.

leave_game(Game) ->
  log(format("~s has left a game.")),
  Game ! {self(), leave}.

game(Game, Turn, GameData) when is_pid(Game) ->
  receive

    % Player messages
    {end_turn, Data} ->
      log(format("~s has ended his turn")),
      Game ! {self(), end_turn, Data},
      game(Game, Turn + 1, GameData);

    {win, _} ->
      log(format("~s has declared that he won. wow.")),
      Game ! {self(), win},
      game(Game, Turn, GameData);

    {lose, _} ->
      log(format("~s has declared that he lost")),
      remove_game(GameData),
      Game ! {self(), lose},
      lose();

    {leave_game, _} ->
      leave_game(Game),
      main();

    disconnected ->
      leave_game(Game),
      stop();

    % Game messages
    {Game, T, turn, Data} when T =< Turn  ->
      log(format("~s received turn data")),
      get(client) ! [turn | Data],
      game(Game, T, GameData);

    {Game, _, you_win, _}->
      remove_game(GameData),
      win();
    {Game, _, draw, _}->
      remove_game(GameData),
      draw()

  after 5000 ->
          game(Game, Turn, GameData)
  end.

%% ENDGAME BEHAVIOR %%
%% TODO optimize db save times

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

get_running_games() ->
  log(format("~s has asked for his games")),
  Games = extract(get(player), games),
  get(client) ! [games, {games, games_to_json(Games)}],
  main().

get_name() ->
  log(format("~s has asked for his name")),
  get(client) ! [send_name, {name, get(name)}],
  main().

games_to_json(Games) ->
  [[{public_id, I}, {name, N}] || {[{I, N}]} <- Games].

to_integer(X) when is_binary(X) ->
  binary_to_integer(X);
to_integer(X)->
  X.

add_game(GameData) ->
  OrigList = extract(get(player), games),
  GameList = [E || {E} <- OrigList],
  FinalList = lists:usort(fun([{K1, _}], [{K2, _}]) -> to_integer(K1) == to_integer(K2) end,
                          [[GameData] | GameList]),
  save_player(replace(get(player), {games, FinalList})).

remove_game({Id, _N}) ->
  OrigList = extract(get(player), games),
  GameList = [E || {E} <- OrigList],
  save_player(replace(get(player), {games, [D || D = [{K, _}] <- GameList, K /= Id]})).

set_rid(P, RID) ->
  db ! {self(), save, replace(P, {registration_id, RID})},
  receive
    {ok, Player} ->
      put(player, Player)
  end.

set_name(Name) ->
  log(format("~s has set his name to ~s", [get(name), Name])),
  save_player(replace(get(player), {name, Name})),
  Name = extract(get(player), name),
  put(name, Name),
  get(client) ! [confirm, {name, Name}],
  main().

set_facebook_id(ID) ->
  log(format("~s has set his facebook id")),
  save_player(replace(get(player), {facebook_id, ID})),
  main().

save_player(P) ->
  db ! {self(), save, P},
  receive
    {ok, Player} ->
      put(player, Player)
  end.

format(Message) ->
  format(Message, [get(name)]).

log(Message) ->
  log(Message, "PLAYER").

%log_error(Message) ->
%log_error(Message, "PLAYER").
