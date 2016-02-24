:- module(bc_string, [
    bc_string_replace/4 % +Atomic, +Atomic, +Atomic, -String
]).

/* <module> String helpers */

:- use_module(library(error)).

%! bc_string_replace(+In, +Search, +Replacement, -Out) is det.
%
% Helper to replace all occurences in
% given string. Out is string.

bc_string_replace(In, Search, Replacement, Out):-
    must_be(atomic, In),
    must_be(atomic, Search),
    must_be(atomic, Replacement),
    atom_string(In, InString),
    atom_string(Search, SearchString),
    atom_string(Replacement, ReplacementString),
    atom_string(InAtom, InString),
    atom_string(SearchAtom, SearchString),
    atomic_list_concat(Tokens, SearchAtom, InAtom),
    atomics_to_string(Tokens, ReplacementString, Out).
