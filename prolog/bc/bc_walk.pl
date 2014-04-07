:- module(bc_walk, [
    bc_walk/3
]).

/** <module> Generic term transform */

:- meta_predicate(bc_walk(2, *, *)).

%! bc_walk(:Handler, +In, -Out) is det.
%
% Generic term transformation.

% FIXME migrate to separate pack?

bc_walk(Handler, In, Out):-
    call(Handler, In, Out), !.

bc_walk(Handler, In, Out):-
    is_list(In), !,
    walk_list(In, Handler, Out).

bc_walk(Handler, In, Out):-
    compound(In), !,
    In =.. [Name|Args],
    length(Args, Len),
    length(ArgsWalked, Len),
    Out =.. [Name|ArgsWalked],
    walk_list(Args, Handler, ArgsWalked).

bc_walk(_, In, In).

:- meta_predicate(walk_list(*, 2, *)).

walk_list([ItemIn|ItemsIn], Handler, [ItemOut|ItemsOut]):-
    bc_walk(Handler, ItemIn, ItemOut),
    walk_list(ItemsIn, Handler, ItemsOut).

walk_list([], _, []).
