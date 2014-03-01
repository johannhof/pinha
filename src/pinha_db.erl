-module(pinha_db).

-import(pinha_utils, [atomize/1, deatomize/1, extract/2]).
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
  listen(Db).

listen(Db) ->
  receive
    {Pid, save, Data} ->
      {Status, {Doc}} = save_doc(Db, {deatomize(Data)}),
      Pid ! {Status, atomize(Doc)},
      listen(Db);

    {Pid, get_registration_id, ID} ->
      case couchbeam_view:first(Db, {"by_public_id", "by_public_id"}, [{key, ID}]) of
        {ok, {[{<<"id">>, Id} | _]}} ->
          {ok, {Doc}} = open_doc(Db, Id),
          Pid ! extract(atomize(Doc), registration_id)
      end,
      listen(Db);

    {Pid, find_friends, Friends} ->
      Keys = [integer_to_binary(X) || X <- Friends],
      case couchbeam_view:fetch(Db, {"by_facebook_id", "by_facebook_id"}, [keys, Keys]) of
        {ok, Elements} ->
          El = [F || {[{_,_}, {_,_}, {<<"value">>,F}]} <- Elements],
          El2 = [[PublicID, Name, PhoneID, Won, Lost, FacebookID] ||
                 {[{<<"_id">>,_},
                   {<<"_rev">>,_},
                   PublicID = {<<"public_id">>, _},
                   Name = {<<"name">>, _},
                   PhoneID = {<<"phone_id">>, _},
                   {<<"registration_id">>, _},
                   FacebookID = {<<"facebook_id">>, _},
                   Won = {<<"won">>,_},
                   Lost = {<<"lost">>,_},
                   {<<"games">>, _}]} <- El],
          Pid ! {friends, atomize(El2)};
        Other ->
          io:fwrite("~p~n", [Other])
      end,
      listen(Db);


    {Pid, get_name, ID} -> % todo enable getting multiple names
      case couchbeam_view:first(Db, {"by_public_id", "by_public_id"}, [{key, ID}]) of
        {ok, {[{<<"id">>, Id} | _]}} ->
          {ok, {Doc}} = open_doc(Db, Id),
          Pid ! {ID, extract(atomize(Doc), name)};
        {ok, nil} -> ok
      end,
      listen(Db);

    {Pid, open_or_create, PhoneId, RID} ->
      case couchbeam_view:first(Db, {"by_phone_id", "by_phone_id"}, [{key, PhoneId}]) of
        {ok, {[{<<"id">>, Id} | _]}} ->
          {ok, {Doc}} = open_doc(Db, Id),
          Pid ! {known_user, atomize(Doc)};
        {ok, nil} ->
          {_, S, MS} = erlang:now(), % generate unique numeric id for games
          {ok, {Doc}} = save_doc(Db,
                                 {[
                                   {public_id, S * 1000 + MS},
                                   {name, <<"Anonymous">>},
                                   {phone_id, PhoneId},
                                   {registration_id, RID},
                                   {facebook_id, <<"">>},
                                   {won, 0},
                                   {lost, 0},
                                   {games, []}
                                  ]}),
          Pid ! {unknown_user, atomize(Doc)}
      end,
      listen(Db)
  end.

stop() -> ok.
