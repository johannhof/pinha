-module(pinha_db).

-import(pinha_utils, [atomize/1, deatomize/1]).
-import(couchbeam, [save_doc/2,
                    open_doc/2,
                    open_or_create_db/3,
                    server_connection/2,
                    server_info/1]).

-export([start/0, stop/0]).

start() ->
  Url = os:getenv("DBURL"),
  Server = server_connection(Url, []),

  % Verify connection
  {ok, _Version} = server_info(Server),

  {ok, Db} = open_or_create_db(Server, "testdb", []),
  put(db, Db),

  listen().

listen() ->
  receive
    {Pid, save, Data} ->
      {Status, {Doc}} = save_doc(get(db), {deatomize(Data)}),
      Pid ! {Status, atomize(Doc)},
      listen();

    {Pid, open_or_create, PhoneId} ->
      case couchbeam_view:first(get(db), {"by_phone_id", "by_phone_id"}, [{key, PhoneId}]) of
        {ok, {[{<<"id">>, Id} | _]}} ->
          {ok, {Doc}} = open_doc(get(db), Id),
          Pid ! {known_user, atomize(Doc)};
        {ok, nil} ->
          {_, S, MS} = erlang:now(), % generate unique numeric id for games
          {ok, {Doc}} = save_doc(get(db),
                                 {[
                                   {public_id, S * 1000 + MS},
                                   {name, <<"Anonymous">>},
                                   {phone_id, PhoneId},
                                   {won, 0},
                                   {lost, 0},
                                   {games, []}
                                  ]}),
          Pid ! {unknown_user, atomize(Doc)}
      end,
      listen()
  end.

stop() -> ok.
