%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Engine.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(ergex_matcher).

%%%_* Exports ==========================================================
%% Constructors
-export([ 'Atom'/0
        , 'Element'/1
        , 'Int'/0
        , 'Nil'/0
        , 'Optional'/1
        , 'Plus'/1
        , 'Seq'/2
        , 'Star'/1
        , 'Alt'/2
        ]).

%% Primitives
-export([ match/2
        ]).

%% Types
-export_type([ col/0
             , re/0
             ]).

%%%_* Code =============================================================
%%%_ * Constructors ----------------------------------------------------
%% A regex G is a pair {Nullable, R}
-type re()                      :: {boolean(), r()}.

%% where the bool Nullable is true iff the empty collection is to be
%% matched, and other matching is determined by R, one of:
-type r()                       :: zero            %matches the empty list
                                 | {element, _X}   %matches one atom or integer
                                 | atom            %matches one atom
                                 | int             %matches one integer
                                 | {seq, _R1, _R2} %matches R1 and R2
                                 | {alt, _R1, _R2} %matches R1 or R2
                                 | {plus, _R}.     %matches at least one R

%% where the R's *must not* match the empty collection, except that one
%% subexpression of a seq may when the other does not.
%% Use only the following constructors, to enforce this invariant.
%% (Termination of the matcher depends on it.)
%% (Many of the cases below are optimizations not needed for correctness
%% -- e.g. the first two seq/2 cases.)
'Nil'()                         -> {true, zero}.

'Element'(X)                    -> {false, {element, X}}.

'Atom'()                        -> {false, atom}.

'Int'()                         -> {false, int}.

'Seq'({N, zero}, G)             -> if N -> G; true -> {false, zero} end;
'Seq'(G, {N, zero})             -> if N -> G; true -> {false, zero} end;
'Seq'({true,  R1}, {N2,    R2}) -> {N2,    {seq, either(one, R1), R2}};
'Seq'({false, R1}, {true,  R2}) -> {false, {seq, R1, either(one, R2)}};
'Seq'({false, R1}, {false, R2}) -> {false, {seq, R1, R2}}.

'Alt'({N1, R1}, {N2, R2})       -> {N1 or N2, either(R1, R2)}.

'Plus'({N, zero})               -> {N, zero};
'Plus'({N, {plus, R}})          -> {N, {plus, R}};
'Plus'({N, R})                  -> {N, {plus, R}}.

%% Rewrite.
'Optional'({_, R})              -> {true, R}.

'Star'({_, R0})                 -> {false, R} = 'Plus'({false, R0}),
                                   {true, R}.

%% Simplify.
-spec either(re(), re())        -> re().
either(zero, R2)                -> R2;
either(R1, zero)                -> R1;
either(R, R)                    -> R;
either(R1, R2)                  -> {alt, R1, R2}.

%%%_ * Primitives ------------------------------------------------------
%% We represent failure by a lazy list of successes.
%% For each operator, m/2 returns all possible remainders. Note that
%% only `alt' and `plus' extend the tail of the result list - all other
%% operators produce singleton-lists only (`seq' touches the elements
%% of the result list, but doesn't change the cardinality of the result
%% list itself.)
-type col()                     :: [atom()|integer()].

-spec match(re(), col())        -> ergex_ll:ll(col()).
match({true,  R}, Xs)           -> m(either(one, R), Xs);
match({false, R}, Xs)           -> m(R, Xs).

-spec m(r(), col())             -> ergex_ll:ll(col()).
m(zero,          _)             -> ergex_ll:new();
m(one,           Xs)            -> ergex_ll:new(Xs);
m({element, X},  [X|Xs])        -> ergex_ll:new(Xs);
m({element, _},  _)             -> ergex_ll:new();
m(atom,          [X|Xs])
  when is_atom(X)               -> ergex_ll:new(Xs);
m(atom,          _)             -> ergex_ll:new();
m(int,           [X|Xs])
  when is_integer(X)            -> ergex_ll:new(Xs);
m(int,           _)             -> ergex_ll:new();
m({seq, R1, R2}, Xs)            -> ergex_ll:flatmap(
                                     fun(Xs1) -> m(R2, Xs1) end,
                                     m(R1, Xs));
m({alt, R1, R2}, Xs)            -> ergex_ll:appent(
                                     m(R1, Xs),
                                     fun() -> m(R2, Xs) end);
m({plus, R},     Xs)            -> ergex_ll:flatmap(
                                     fun(Xs1) ->
                                       [Xs1|fun() -> m({plus, R}, Xs1) end]
                                     end,
                                     m(R, Xs)).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rewriting_test() ->
  Nil  = 'Nil'(),
  Foo  = 'Element'(foo),
  FooO = 'Optional'(Foo),
  Fail = {false, zero},

  Nil  = 'Plus'(Nil),
  FooP = 'Plus'(Foo),
  FooP = 'Plus'('Plus'('Plus'(FooP))),

  Fail = 'Seq'(Fail, Foo),
  Fail = 'Seq'(Foo, Fail),

  _    = 'Seq'(FooO, Foo),
  _    = 'Seq'(Foo, FooO),

  Foo  = 'Alt'(Foo, Foo),
  Nil  = 'Alt'(Fail, Nil),

  ok.


-define(MATCH(RE, Col), ergex_ll:to_list(match(RE, Col))).

nil_test() ->
  [[]] = ?MATCH('Nil'(), []),
  []   = ?MATCH({false, zero}, []).

element_test() ->
  [[]] = ?MATCH('Element'(foo), [foo]).

atom_test() ->
  [[]] = ?MATCH('Atom'(), [foo]),
  []   = ?MATCH('Atom'(), [42]).

int_test() ->
  [[]] = ?MATCH('Int'(), [42]),
  []   = ?MATCH('Int'(),   [foo]).

seq_test() ->
  [[]] = ?MATCH('Seq'('Element'(foo), 'Element'(bar)), [foo, bar]).

alt_test() ->
  [[]] = ?MATCH('Alt'('Element'(foo), 'Element'(bar)), [foo]),
  [[]] = ?MATCH('Alt'('Element'(foo), 'Element'(bar)), [bar]).

plus_test() ->
  [[]]        = ?MATCH('Plus'('Element'(foo)), [foo]),
  [[foo], []] = ?MATCH('Plus'('Element'(foo)), [foo, foo]).

optional_test() ->
  [[]]        = ?MATCH('Optional'('Element'(foo)), []),
  [[foo], []] = ?MATCH('Optional'('Element'(foo)), [foo]).

star_test() ->
  [[]]        = ?MATCH('Star'('Element'(foo)), []),
  [[foo], []] = ?MATCH('Star'('Element'(foo)), [foo]).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
