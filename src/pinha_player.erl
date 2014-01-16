-module(pinha_player).

-import(pinha_utils, [replace/2, extract/2, log/2, log_error/2]).

-import(io_lib, [format/2]).

-export([start/1, stop/0]).

% Process memory:
% client - Pid of the handler that sends messages to the client
% player - database object of the player
% name - name of the player

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

    {pair, [{partner, PartnerID} | _]} ->
      lobby ! {self(), pair, PartnerID, get(name)},
      receive
        {Pid, accept} ->
          lobby ! {self(), remove},
          get(client) ! [start],
          game(Pid);
        {_Pid, deny} ->
          get(client) ! [denied],
          lobby();
        not_found ->
          get(client) ! [denied],
          lobby()
      end,
      lobby();

    {request, Partner, Name} ->
      get(client) ! [request, {partner, Name} ],
      log(format("~s wants to pair with ~s", [Name, get(name)])),
      receive
        {accept, _} ->
          Partner ! {self(), accept},
          lobby ! {self(), remove},
          log(format("~s has accepted")),
          game(Partner);
        {deny, _} ->
          Partner ! {self(), deny},
          log(format("~s has denied")),
          lobby();
        disconnected ->
          Partner ! {self(), deny},
          lobby ! {self(), remove},
          general(disconnected, {})
      end;

    Update = [lobby_update | _Data] ->
      get(client) ! Update,
      lobby();

    disconnected ->
      lobby ! {self(), remove},
      general(disconnected, {});

    Other ->
      general(Other, fun lobby/0)
  end.

game(Partner) ->
  receive % here the players are calculating the opponent's results
    [lobby_update | _] -> game(Partner); % in case any lobby updates came through
    [leave_lobby | _] -> game(Partner); % in case any lobby updates came through

    {ready, _} ->
      log(format("~s is ready")),
      Partner ! ready,
      receive % waiting for the opponent's ready
        ready ->
          get(client) ! [turn],
          log(format("~s starts his turn")),
          receive %players are calculating their own turn now
            {end_turn, Data} ->
              log(format("~s has ended his turn")),
              Partner ! [end_turn | Data],
              receive
                D = [end_turn | _] -> get(client) ! D;
                you_win -> win(Partner)
              end,
              game(Partner);
            {lose, _} ->
              log(format("~s has resigned")),
              Partner ! you_win,
              lose(Partner);
            you_win -> win(Partner)
          end;
        you_win -> win(Partner)
      end;

    {lose, _} ->
      log(format("~s has declared that he lost")),
      Partner ! you_win,
      receive
        you_win ->
          log(format("~s has tied? Woot?")),
          get(client) ! [draw],
          gameover(Partner);
        ready -> lose(Partner)
      end,
      lobby();

    partner_left ->
      log(format("~s is shocked to hear this.")),
      get(client) ! [partner_left],
      lobby();

    disconnected ->
      log(format("~s has left a running game. Notifiying partner")),
      Partner ! partner_left,
      general(disconnected, {})
  end.

lose(Partner) ->
  Losses = extract(get(player), lost),
  db ! {self(), save, replace(get(player), {lost, Losses + 1})},
  receive
    {ok, Player} ->
      put(player, Player)
  end,
  gameover(Partner).

win(Partner) ->
  log(format("~s has won the game. Yeah!")),
  get(client) ! [you_win],
  Wins = extract(get(player), won),
  db ! {self(), save, replace(get(player), {won, Wins + 1})},
  receive
    {ok, Player} ->
      put(player, Player)
  end,
  gameover(Partner).

gameover(Partner) ->
  receive

    {revenge, _} ->
      log(format("~s wants revenge")),
      Partner ! revenge,
      receive
        accept ->
          get(client) ! [start],
          game(Partner);
        _ ->
          get(client) ! [denied],
          gameover(Partner)
      end;

    revenge ->
      get(client) ! [request],
      receive
        {accept, _} ->
          Partner ! accept,
          log(format("~s has accepted")),
          game(Partner);
        {deny, _} ->
          Partner ! deny,
          log(format("~s has denied")),
          gameover(Partner)
      end;

    partner_left_game_over ->
      get(client) ! [partner_left_game_over],
      nowhere();

    Other ->
      Partner ! partner_left_game_over,
      log(format("~s has left the game over area")),
      self() ! Other,
      nowhere()
  end.

general({set_name, [{name, Name} | _]}, F) ->
  log(format("~s has set his name to ~s", [get(name), Name])),
  db ! {self(), save, replace(get(player), {name, Name})},
  receive
    {ok, Player} ->
      put(name, Name),
      put(player, Player),
      get(client) ! [confirm, {name, Name}]
  end,
  F();

general({get_name, _}, F) ->
  log(format("~s has asked for his name")),
  get(client) ! [send_name, {name, get(name)}],
  F();

% unknown action
general({Action, _}, F) when is_atom(Action) ->
  {name, Name} = erlang:fun_info(F, name),
  log_error(format("Client has the status ~s. It can not receive the action ~s", [Name, Action])),
  F();

general(disconnected, _) ->
  stop();

% keep calm and carry on
general(_, F) ->
  F().

start(Pid) ->
  log("A client connected"),
  put(client, Pid),
  unidentified().

stop() ->
  log(format("~s has disconnected", [get(name)])),
  exit(normal).

format(Message) ->
  format(Message, [get(name)]).

log(Message) ->
  log(Message, "PLAYER").

log_error(Message) ->
  log_error(Message, "PLAYER").
