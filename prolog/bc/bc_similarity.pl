:- module(bc_similarity, [
    bc_similar_ids/3 % +Type, +Id, -List
]).

/** <module> Entry similarity analysis */

:- use_module(library(docstore)).
:- use_module(library(sort_dict)).
:- use_module(library(ordsets)).

:- use_module(bc_search).

%! bc_similar_ids(+Type, +Id, -List) is det.
%
% Finds similar entries of the
% type for the given entry.

bc_similar_ids(Type, Id, List):-
    must_be(atom, Type),
    must_be(atom, Id),
    similarity_list(Type, Id, List).

similarity_list(Type, Id, Sorted):-
    (   ds_col_get(entry, Id, [tags], Entry)
    ->  ds_find(entry, (published=true, type=Type),
            [title, tags], Entries),
        include(common_tag(Entry.tags), Entries, Filtered),
        maplist(entry_cosine_similarity(Id), Filtered, Similarities),
        sort_dict(score, desc, Similarities, Sorted)
    ;   throw(error(no_entry(Id), _))).

% Succeeds when the entry has a common
% tag with the list of tags.

common_tag(Tags, Entry):-
    member(Tag, Tags),
    member(Tag, Entry.tags).

% Finds the entry similarity score
% for the given tags. Gives back a dict.

entry_cosine_similarity(Id1, Entry, Similarity):-
    ds_id(Entry, Id2),
    bc_cosine_similarity(Id1, Id2, Cosine),
    Similarity = _{
        entry: Id2,
        score: Cosine,
        title: Entry.title }.
