:- module(bc_util, [
    excerpt/2,
    excerpt/3
]).

:- use_module(bc_config).

%% excerpt(+Html, -Excerpt) is det.
%
% Same as excerpt/3 but reads excerpt
% size from the configuration parameter
% excerpt_size.

excerpt(Html, Excerpt):-
    config_get(excerpt_size, Length),
    excerpt(Html, Length, Excerpt).

%% excerpt(+Html, Length, -Excerpt) is det.
%
% Extracts prefix with the given length.
% Removes all tags.

% FIXME rewrite using dcg.

excerpt(Html, Length, Excerpt):-
    atom_codes(Html, Codes),
    excerpt(Codes, Length, [], ExcerptCodes),
    atom_codes(Excerpt, ExcerptCodes).

excerpt([], _, SoFar, Excerpt):-
    reverse(SoFar, Excerpt), !.

excerpt(_, Length, SoFar, Excerpt):-
    Length =< 0, !,
    reverse([46,46,46,32|SoFar], Excerpt).

excerpt([60|Codes1], Length, SoFar, Excerpt):-
    excerpt_tag(Codes1, Codes2), !,
    excerpt(Codes2, Length, SoFar, Excerpt).

excerpt([Code|Codes], Length1, SoFar, Excerpt):-
    Length2 is Length1 - 1,
    excerpt(Codes, Length2, [Code|SoFar], Excerpt).

excerpt_tag([62|Codes], Codes):- !.
excerpt_tag([_|Codes1], Codes2):-
    excerpt_tag(Codes1, Codes2).
