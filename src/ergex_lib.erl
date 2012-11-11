%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Utility library.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(ergex_lib).

%%%_* Exports ==========================================================
-export([ consult_string/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
-spec consult_string(string()) -> {ok, _} | {error, _}.
%% @doc Parse String as an Erlang term.
consult_string(String) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _}    -> erl_parse:parse_term(Tokens);
    {error, Info, Loc} -> {error, {Info, Loc}}
  end.

consult_string_test() ->
  {ok, 42}   = consult_string("42"),
  {error, _} = consult_string("'"),
  {error, _} = consult_string("{42"),
  ok.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
