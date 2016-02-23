:- module(bc_list, [
    take_prefix/3 % +Length, +List, -Prefix
]).

% Helper to take N first elements
% from the given list.

take_prefix(N, List, Prefix):-
    must_be(integer, N),
    must_be(list, List),
    take(N, List, Prefix).

take(N, List, Prefix):-
    (   N =< 0
    ->  Prefix = []
    ;   N1 is N - 1,
        (   List = [Head|Tail]
        ->  Prefix = [Head|Rest],
            take(N1, Tail, Rest)
        ;   Prefix = [])).
