%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc API.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(ergex).

%%%_* Exports ==========================================================
-export([ all_matches/2
        , matches_p/2
        ]).

-export_type([ collection/0
             , regex/0
             ]).

%%%_* Code =============================================================
-type regex()      :: string().
-type collection() :: [atom()|integer()]
                    | {atom()|integer()}.

-spec all_matches(regex(), collection()) -> [collection()].
%% @doc Returns the tails of the input after all successful matches.
all_matches(Regex, Collection) -> ergex_ll:to_list(match(Regex, Collection)).

-spec matches_p(regex(), collection()) -> boolean().
%% @doc Return true iff Collection matches Regex.
matches_p(Regex, Collection) -> ergex_ll:member([], match(Regex, Collection)).


match(Regex, Collection) ->
  {Tag, RE} = ergex_parser:parse(Regex),
  Col       = to_list(Tag, Collection),
  ergex_matcher:match(RE, Col).

to_list(list,  X) -> X;
to_list(tuple, X) -> tuple_to_list(X).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

list_test() ->
  true         = matches_p("[]",                        []),
  false        = matches_p("[foo,bar]",                 [foo,bar,bar,quux]),
  [[bar,quux]] = all_matches("[foo,bar]",               [foo,bar,bar,quux]),
  []           = all_matches("[foo,bar+,quux*,<atom>]", [foo,bar]),
  [[]]         = all_matches("[foo,bar+,quux*,<atom>]", [foo,bar,bar]),
  [[]]         = all_matches("[foo,bar+,quux*,<atom>]", [foo,bar,snarf]),
  [[snarf],[]] = all_matches("[foo,bar+,quux*,<atom>]", [foo,bar,bar,snarf]),
  [[quux],[]]  = all_matches("[foo,bar+,quux*,<atom>]", [foo,bar,bar,quux]),
  []           = all_matches("[foo,bar+,quux,<atom>]",  [foo,bar,bar,quux]),
  ok.

tuple_test() ->
  true                  = matches_p("{}",                      {}),
  false                 = matches_p("{1}",                     {1,2}),
  [[2]]                 = all_matches("{1}",                   {1,2}),
  []                    = all_matches("{0|1,42,666?,<int>**}", {}),
  [[]]                  = all_matches("{0|1,42,666?,<int>**}", {0,42}),
  [[666],[]]            = all_matches("{0|1,42,666?,<int>**}", {1,42,666}),
  [[666,23],[23],[],[]] = all_matches("{0|1,42,666?,<int>**}", {1,42,666,23}),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
