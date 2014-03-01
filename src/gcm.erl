-module(gcm).
-import(jsx, [encode/1]).
-import(pinha_utils, [log/2]).

-export([msg/2]).

-define(KEY, "key=AIzaSyAdMoslAbCj963AcLa3TGZa8eV_QLSkM60").
-define(URL, "http://android.googleapis.com/gcm/send").

msg(ID, Message) ->
  db ! {self(), get_registration_id, ID},
  receive
    RID ->
      log("Sending message..."),
      try
        Body = encode([{registration_ids, [RID]}, {data, Message}]),
        post(Body)
      catch
        Exception -> erlang:display(Exception),
                     erlang:display([{registration_ids, [RID]}, {data, Message}])
      end
  end.

post(Body) ->
  inets:start(),
  try httpc:request(post,
                    {?URL, [{"Authorization", ?KEY}], "application/json", Body},
                    [],
                    [])
  of
    {ok, {{_,200,_},_,_RespBody}} ->
      log("...success!");
    {error, Reason } ->
      log("...error!"),
      erlang:display(Reason);
    {ok, {{StatusLine,_,_},_,RespBody}} ->
      log("...error!"),
      erlang:display({StatusLine, RespBody});
    BigError -> {error, BigError}
  catch
    Throw ->
      erlang:display(Throw)
  end.

log(Message) ->
  log(Message, "GCM").
