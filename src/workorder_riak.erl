-module(workorder_riak).

-export([body/1]).
-export([set_body/2]).
-export([set_binary_index/3]).
-export([has_binary_index/3]).

body(Obj) ->
  case riakc_obj:get_content_type(Obj) of
    "application/x-erlang-term" ->
      try
        binary_to_term(riakc_obj:get_value(Obj))
      catch
        _:Error ->
          {error, Error}
      end;
    Ctype ->
      {error, {unknown_content_type, Ctype}}
  end.

set_body(Value, Obj) ->  
  riakc_obj:update_value(Obj, term_to_binary(Value, [compressed]), <<"application/x-erlang-term">>).

set_binary_index(Index, Value, Obj) ->
  MD = riakc_obj:get_update_metadata(Obj),
  MD2 = riakc_obj:set_secondary_index(MD, [{{binary_index, Index}, [Value]}]),
  riakc_obj:update_metadata(Obj, MD2).

has_binary_index(Index, Value, Obj) ->
  MD = riakc_obj:get_metadata(Obj),
  List = riakc_obj:get_secondary_index(MD, {binary_index, Index}),
  List =:= [Value].
