:- module(bc_comment_mention, [
    bc_mentions_parse/3 % +Content, +Names, -Mentions
]).

:- use_module(library(error)).
:- use_module(library(dcg/basics)).

%! bc_mentions_parse(+Content, +Names, -Mentions) is det.
%
% Parses mentions in content. Uses names list.

bc_mentions_parse(Content, Names, Mentions):-
    must_be(string, Content),
    must_be(list, Names),
    maplist(normalize, Names, NamePairs),
    string_codes(Content, ContentCodes),
    phrase(mentions(NamePairs, Tmp), ContentCodes), !,
    Tmp = Mentions.

normalize(Name, Name-NormalizedCodes):-
    string_codes(Name, Codes),
    normalize_codes(Codes, NormalizedCodes).

normalize_codes(Codes, Normalized):-
    maplist(to_lower, Codes, Lower),
    exclude(stripped, Lower, Normalized).

stripped(Code):-
    \+ code_type(Code, alnum).

mentions(NamePairs, [Mention|Mentions]) -->
    mention(NamePairs, Mention), !,
    mentions(NamePairs, Mentions).

mentions(NamePairs, Mentions) -->
    [_], !, mentions(NamePairs, Mentions).

mentions(_, []) --> [].

mention(NamePairs, Name) -->
    "@", name_match(NamePairs, Name).

name_match(Names, Original) -->
    { member(Original-Test, Names) },
    name_match(Test), !.

name_match([Lower|Codes]) -->
    [Code], { to_lower(Code, Lower) },
    name_match(Codes).

name_match(Codes) -->
    { Codes \= [] },
    white, name_match(Codes).

name_match([]) --> [].
