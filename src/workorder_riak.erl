-module(workorder_riak).

-export([body/1]).
-export([set_body/2]).

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

