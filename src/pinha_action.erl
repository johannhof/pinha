-module(pinha_action).

-export([server_action/1, client_action/1]).

server_action(lobby_update)->1001;
server_action(confirm)->1006;
server_action(unknown_user)->1008;
server_action(known_user)->1010;
server_action(send_name)->1011.

client_action(2001)->set_name;
client_action(2002)->go_to_lobby;
client_action(2003)->pair;
client_action(2007)->set_id;
client_action(2009)->get_name;
client_action(2011)->leave_lobby;
client_action(_)->invalid.
