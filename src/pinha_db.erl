-module(pinha_db).

-import(pinha_utils, [atomize/1, deatomize/1]).

-export([start/0, stop/0]).

start() ->
  Url = "http://localhost:5984",
  Server = couchbeam:server_connection(Url, []),

  % Verify connection
  {ok, _Version} = couchbeam:server_info(Server),

  {ok, Db} = couchbeam:open_or_create_db(Server, "testdb", []),
  put(db, Db),

  listen().

listen() ->
  receive
    {Pid, save, Data} ->
      {Status, {Doc}} = couchbeam:save_doc(get(db), {deatomize(Data)}),
      Pid ! {Status, atomize(Doc)},
      listen();

    {Pid, open_or_create, PhoneId} ->
      case couchbeam_view:first(get(db), {"by_phone_id", "by_phone_id"}, [{key, PhoneId}]) of
        {ok, {[{<<"id">>, Id} | _]}} ->
          {ok, {Doc}} = couchbeam:open_doc(get(db), Id),
          Pid ! {known_user, atomize(Doc)};
        {ok, nil} ->
          {ok, {Doc}} = couchbeam:save_doc(get(db), {[{name, <<"Anonymous">>}, {phone_id, PhoneId}, {won, 0}, {lost, 0}]}),
          Pid ! {unknown_user, atomize(Doc)}
      end,
      listen()
  end.

stop() ->
  ok.
