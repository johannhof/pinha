-module(pinha_utils).

-export([atomize/1,
         deatomize/1,
         replace/2,
         extract/2,
         formatted_now/0,
         log/2,
         log_error/2
        ]).

atomize({K,V}) when is_binary(K)-> {binary_to_atom(K, utf8), V};
atomize(L) when is_list(L)-> [atomize(E) || E <- L];
atomize(E)-> E.

deatomize({K,V}) when is_atom(K)-> {atom_to_binary(K, utf8), V};
deatomize(L) when is_list(L)-> [deatomize(E) || E <- L];
deatomize(E)-> E.

replace(List, Element) ->
  replace(List, Element, []).

replace([], _Element, L) ->
  lists:reverse(L);
replace([{OKey, _OValue} | T], {Key, Value}, L) when OKey =:= Key ->
  replace(T, {Key, Value}, [{Key, Value} | L]);
replace([OElement | T], Element, L) ->
  replace(T, Element, [OElement | L]).

extract([{K, V} | _T], Key) when Key =:= K -> V;
extract([{_K, _V} | T], Key) -> extract(T, Key).

formatted_now() ->
  {{Year,Month,Day},{Hour,Minutes,Seconds}} = erlang:localtime(),
  io_lib:format("~w/~w/~w ~w:~w:~w", [Year,Month,Day,Hour,Minutes,Seconds]).

log_error(Message, From) ->
  io:format("~s [ERROR|~s] ~s.~n", [formatted_now(), From, Message]).

log(Message, From) ->
  io:format("~s [INFO|~s] ~s.~n", [formatted_now(), From, Message]).
