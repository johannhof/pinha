-module(pinha_game).

-export([start/3, stop/0]).

-record(player, {pid=0::pid(), id::string(), turn=1, turns}).

end_turn({Player = #player{turn=Turn, turns=Turns}, Opponent}, Data) ->
  send_or_gcm(Opponent, turn, Data), % send turn data to opponent
  ets:insert(Turns, {Turn, Data}), % save turn data into ets
  {Player#player{turn=Turn+1}, Opponent}. % return new data

enter_player({Player, Opponent}, Pid) ->
  {Player#player{pid=Pid}, Opponent}.

remove_player({Player, Opponent}) ->
  {Player#player{pid=0}, Opponent}.

main(Players) ->
  receive
    {Pid, end_turn, Data} ->
      main(end_turn(with(Pid, Players), Data));
    {Pid, enter, ID} ->
      main(enter_player(with_id(ID, Players), Pid));
    {Pid, leave} ->
      main(remove_player(with(Pid, Players)))
  end.

send_or_gcm(Player, Message, Data) ->
  case Player#player.pid of
      0 -> ok; % TODO gcm
      Pid -> Pid ! {self(), Message, Data}
  end.

% transform player tuple so that the player with the Pid is first
with(Pid, {Player1, Player2})->
  if Player1#player.pid =:= Pid ->
       { Player1, Player2 };
     Player2#player.pid =:= Pid ->
       { Player2, Player1 }
  end.

with_id(ID, {Player1, Player2})->
  if Player1#player.id =:= ID ->
       { Player1, Player2 };
     Player2#player.id =:= ID ->
       { Player2, Player1 }
  end.

start(new, Pid, {P1ID, P2ID}) ->
  Tab1 = ets:new(player1_table, [private, ordered_set]),
  Tab2 = ets:new(player2_table, [private, ordered_set]),
  main({
    #player{pid=Pid, id=P1ID, turns=Tab1},
    #player{id=P2ID, turns=Tab2}
   }).

stop() -> ok.
