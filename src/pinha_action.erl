-module(pinha_action).

-export([server_action/1, client_action/1]).

server_action(lobby_update)->1001;
server_action(new_game)->1002;
server_action(existing_game)->1003;
server_action(turn)->1004;
server_action(confirm)->1005;
server_action(unknown_user)->1006;
server_action(you_win)->1007;
server_action(known_user)->1008;
server_action(send_name)->1009;
server_action(draw)->1010;
server_action(games)->1011;
server_action(friends)->1012.

client_action(2001)->set_name;
client_action(2002)->go_to_lobby;
client_action(2003)->enter_game;
client_action(2004)->set_id;
client_action(2005)->lose;
client_action(2006)->get_name;
client_action(2007)->end_turn;
client_action(2008)->leave_lobby;
client_action(2009)->leave_game;
client_action(2010)->get_running_games;
client_action(2011)->win;
client_action(2012)->find_friends;
client_action(2013)->facebook_id;
client_action(_)->invalid.
