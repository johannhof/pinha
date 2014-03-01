-module(pinha_game).

-export([start/4]).

-import(pinha_utils, [log/2]).

-record(player, {pid=off::pid(),
                 id::string(),
                 name::string(),
                 left::boolean(),
                 turn=1::integer(),
                 turns}).

end_turn({Player = #player{turn=Turn, turns=Turns},
          Opponent = #player{turn=OTurn}}, Data) when Turn =< OTurn ->
  log("Sending turn data to opponent"),
  send_or_gcm(Opponent, Player, Turn, turn, Data), % send turn data to opponent
  ets:insert(Turns, {Turn, Data}), % save turn data into ets
  {Player#player{turn=Turn+1}, Opponent}; % return new data
end_turn(Players,_) ->
  log("Warning: a client tried have a turn too early"),
  Players. % return new data

player_as_json(#player{left=Left, turn=Turn, turns=Turns, name=Name}) ->
  Data = case ets:lookup(Turns, Turn - 1) of [{_, D}] -> D; [] -> [{}] end,
  [ {left, Left}, {turn, Turn}, {name, Name}, {data, Data} ].

enter_player({Player = #player{turn=Turn}, Opponent}, Pid) ->
  log("A Player entered the game"),
  %erlang:display(Pid),
  PNew = Player#player{pid=Pid},
  send_or_gcm(PNew, Opponent, Turn, existing_game, {{Opponent#player.id, Opponent#player.name},
                                              [{you, player_as_json(PNew)},
                                               {opponent, player_as_json(Opponent)}]
                                             }),
  %erlang:display({PNew, Opponent}),
  {PNew, Opponent}.

remove_player({Player, Opponent}) ->
  {Player#player{pid=off}, Opponent}.

main(Players) ->
  receive
    {Pid, end_turn, Data} ->
      main(end_turn(with(Pid, Players), Data));
    {Pid, enter, ID} ->
      main(enter_player(with_id(ID, Players), Pid));
    {Pid, leave} ->
      main(remove_player(with(Pid, Players)));
    {Pid, lose} ->
      stop(with(Pid, Players));
    {_Pid, win} ->
      stop(with_id(1, Players))

  end.

send_or_gcm(#player{pid=Pid}, _O, Turn, Message, Data) when is_pid(Pid) ->
  log("Sending by websocket"),
  Pid ! {self(), Turn, Message, Data};
send_or_gcm(#player{id=ID}, #player{id=OID}, _, Message, _Data) when ID /= 1 ->
  log("Sending by gcm"),
  gcm:msg(ID, [{msg, atom_to_binary(Message, utf8)}, {id, OID}]);
send_or_gcm(_, _, _, _, _) ->
  ok.

% transform player tuple so that the player with the Pid is first
with(Pid, {Player1, Player2})->
  %erlang:display(Pid),
  %erlang:display({Player1, Player2}),
  if Player1#player.pid =:= Pid ->
       { Player1, Player2 };
     Player2#player.pid =:= Pid ->
       { Player2, Player1 }
  end.

with_id(ID, {Player1, Player2})->
  erlang:display(ID),
  erlang:display({Player1, Player2}),
  if Player1#player.id =:= ID ->
       {Player1, Player2};
     Player2#player.id =:= ID ->
       {Player2, Player1}
  end.

get_player_name(ID) when ID == 1 ->
  <<"Dummy">>;

%% TODO this is hell. make more efficient
get_player_name(ID) ->
  db ! {self(), get_name, ID},
  receive {ID, Name} -> Name end.

start(new, GameID, Pid, {P1ID, P2ID}) ->
  put(id, GameID),
  %erlang:display(Pid),
  Tab1 = ets:new(player1_table, [private, ordered_set]),
  Tab2 = ets:new(player2_table, [private, ordered_set]),
  P1Name = get_player_name(P1ID),
  P2Name = get_player_name(P2ID),
  Player1 = #player{pid=Pid, id=P1ID, name=P1Name, turns=Tab1, left=false},
  Player2 = #player{name=P2Name, id=P2ID, turns=Tab2, left=true},
  send_or_gcm(Player2, Player1, 1, new_game, {{P1ID, Player1#player.name},
                                              [{you, player_as_json(Player2)},
                                               {opponent, player_as_json(Player1)}]
                                             }),
  send_or_gcm(Player1, Player2, 1, new_game, {{P2ID, Player2#player.name},
                                              [{you, player_as_json(Player1)},
                                               {opponent, player_as_json(Player2)}]
                                             }),
  main({Player1, Player2}).

stop({Loser, Winner}) ->
  send_or_gcm(Winner, Loser, 0, you_win, {}),
  {_, S, MS} = erlang:now(), % generate unique numeric id for games
  game_service ! {get(id), end_game, {S * 1000 + MS,
                                      Winner#player{turns=ets:tab2list(Winner#player.turns)},
                                      Loser}}.

log(Message) ->
  log(Message, "GAME").
