:- module(bc_excerpt, [
    bc_excerpt/3 % +Html, +Size, -Excerpt
]).

:- use_module(library(dcg/basics)).

%! bc_excerpt(+Html, +Size, -Excerpt) is det.
%
% Provides excerpt from HTML by removing tags
% and taking prefix of text.

bc_excerpt(Html, Size, Excerpt):-
    string_codes(Html, Codes),
    phrase(excerpt(ExcerptCodes, Size), Codes, _),
    string_codes(Excerpt, ExcerptCodes).

excerpt(Codes, Size) -->
    "<", !, excerpt_tag,
    excerpt(Codes, Size).

excerpt([Code|Codes], Size) -->
    [Code], {
        Size > 0,
        SizeNext is Size - 1
    }, !,
    excerpt(Codes, SizeNext).

excerpt([], 0) --> !, "".

excerpt([], _) --> eos.

excerpt_tag -->
    ">", !.

excerpt_tag -->
    [_], excerpt_tag.
