-module(pinha_action).

-export([server_action/1, client_action/1]).

server_action(lobby_update)->1001;
server_action(new_game)->1002;
server_action(existing_game)->1003;
%server_action(denied)->1004;
server_action(turn)->1005;
server_action(confirm)->1006;
%server_action(partner_left)->1007;
server_action(unknown_user)->1008;
server_action(you_win)->1009;
server_action(known_user)->1010;
server_action(send_name)->1011;
%server_action(end_turn)->1012;
%server_action(partner_left_game_over)->1013;
server_action(draw)->1014.

client_action(2001)->set_name;
client_action(2002)->go_to_lobby;
client_action(2003)->enter_game;
client_action(2004)->set_id;
client_action(2005)->lose;
client_action(2006)->get_name;
client_action(2007)->end_turn;
client_action(2008)->leave_lobby;
client_action(_)->invalid.
