%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Convert string representation of a regex into re() representation of
%%% a regex.
%%% @todo ()-groups
%%% @todo support more Erlang terms
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(ergex_parser).

%%%_* Exports ==========================================================
-export([ parse/1
        ]).

%%%_* Imports ==========================================================
-import(ergex_matcher,
        [ 'Atom'/0
        , 'Element'/1
        , 'Int'/0
        , 'Nil'/0
        , 'Optional'/1
        , 'Plus'/1
        , 'Seq'/2
        , 'Star'/1
        , 'Alt'/2
        ]).

%%%_* Code =============================================================
-spec parse(string()) -> {list|tuple, ergex_matcher:re()}.
parse(Xs0)            -> case unparen(Xs0) of
                           {"[]", Xs} -> {list,  do_parse(Xs)};
                           {"{}", Xs} -> {tuple, do_parse(Xs)}
                         end.

unparen([O|Xs])       -> [C|Sx] = lists:reverse(Xs),
                         {[O, C], lists:reverse(Sx)}.

do_parse(Xs)          -> lists:foldr(fun ergex_matcher:'Seq'/2,
                                     'Nil'(),
                                     [re(X) || X <- split(Xs)]).

split(Xs)             -> lists:map(fun string:strip/1, string:tokens(Xs, ",")).

re(X)                 -> re(X, 'Nil'()).

re("|"++Xs,      RE)  -> 'Alt'(RE, re(Xs));
re("*"++Xs,      RE)  -> re(Xs, 'Star'(RE));
re("+"++Xs,      RE)  -> re(Xs, 'Plus'(RE));
re("?"++Xs,      RE)  -> re(Xs, 'Optional'(RE));
re("<atom>"++Xs, RE)  -> RE = 'Nil'(),
                         re(Xs, 'Atom'());
re("<int>"++Xs,  RE)  -> RE = 'Nil'(),
                         re(Xs, 'Int'());
re([_|_] = Xs,   RE)  -> RE = 'Nil'(),
                         {Elt, Rest} = elt(Xs),
                         re(Rest, 'Element'(Elt));
re("",           RE)  -> RE.

elt(Xs)               -> {Elt0, Rest} = lists:splitwith(fun is_elt/1, Xs),
                         {ok, Elt}    = ergex_lib:consult_string(Elt0),
                         {Elt, Rest}.

is_elt(X)             -> lists:member(X, "abcdefghijklmnopqrstuvwxyz"
                                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                         "0123456789"
                                         "_").

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
  Exp1 = {list, 'Seq'('Star'('Element'(foo)),
                      'Seq'('Element'(bar),
                            'Alt'('Optional'('Element'(baz)), 'Int'())))},
  Res1 = parse("[foo*, bar, baz?|<int>]"),
  ?assertEqual(Exp1, Res1),

  Exp2 = {tuple, 'Seq'('Plus'('Element'(42)),
                       'Seq'('Element'(0),
                             'Star'('Atom'())))},
  Res2 = parse("{42+, 0, <atom>*}"),
  ?assertEqual(Exp2, Res2),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
