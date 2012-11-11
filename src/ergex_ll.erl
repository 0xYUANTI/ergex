%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lazy lists.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(ergex_ll).

%%%_* Exports ==========================================================
-export([ appent/2
        , cons/2
        , flatmap/2
        , member/2
        , new/0
        , new/1
        , to_list/1
        ]).

-export_type([ ll/1
             , head/1
             , tail/1
             ]).

%%%_* Code =============================================================
-type ll(A)                             :: []
                                         | [head(A) | tail(A)].
-type head(A)                           :: A.
-type tail(A)                           :: fun(() -> ll(A)).

-spec to_list(ll(A))                    -> [A].
to_list([])                             -> [];
to_list([H|T])                          -> [H|to_list(T())].

-spec member(A, ll(A))                  -> boolean().
member(X, [X|_])                        -> true;
member(X, [_|Xs])                       -> member(X, Xs());
member(_, [])                           -> false.

-spec new()                             -> ll(_).
new()                                   -> [].

-spec new(A)                            -> ll(A).
new(H)                                  -> [H|fun() -> [] end].

-spec cons(A, ll(A))                    -> ll(A).
cons(H, T)                              -> [H|fun() -> T end].

-spec flatmap(fun((A) -> ll(A)), ll(A)) -> ll(A).
flatmap(_, [])                          -> [];
flatmap(F, [H|T])                       -> appent(
                                             F(H),
                                             fun() -> flatmap(F, T()) end).

-spec appent(ll(A), tail(A))            -> ll(A).
appent([],     T2)                      -> T2();
appent([H|T1], T2)                      -> [H|fun() -> appent(T1(), T2) end].

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  [foo, bar, baz] = to_list(cons(foo, cons(bar, new(baz)))),
  ok.

member_test() ->
  true  = member(bar, cons(foo, cons(bar, new(baz)))),
  false = member(quux, cons(foo, cons(bar, new(baz)))),
  ok.

flatmap_test() ->
  Mapper    =
    fun(0) -> [];
       (1) -> new(1);
       (2) -> cons(1, new(2))
    end,
  Ll        = cons(0, cons(1, cons(2, new()))),
  [1, 1, 2] = to_list(flatmap(Mapper,  Ll)),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
