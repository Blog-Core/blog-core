:- module(bc_similarity, [
    bc_similar/3 % +Type, +Id, -List
]).

/** <module> Entry similarity analysis */

:- use_module(library(docstore)).
:- use_module(library(sort_dict)).

:- use_module(bc_search).
:- use_module(bc_excerpt).

%! bc_similar(+Type, +Id, -List) is det.
%
% Finds similar entries of the
% type for the given entry.

bc_similar(Type, Id, List):-
    must_be(atom, Type),
    must_be(atom, Id),
    similarity_list(Type, Id, List).

similarity_list(Type, Id, Results):-
    (   ds_col_get(entry, Id, [tags], Entry)
    ->  ds_find(entry, (published=true, type=Type),
            [slug, tags, title, author, date_published,
            date_updated, description, language], Entries),
        include(common_tag(Entry.tags), Entries, WithCommonTag),
        exclude(same_entry(Id), WithCommonTag, Filtered),
        maplist(entry_cosine_similarity(Id), Filtered, Similarities),
        sort_dict(score, desc, Similarities, Sorted),
        maplist(add_extra_data, Sorted, Results)
    ;   throw(error(no_entry(Id), _))).

% Adds excerpt.

add_extra_data(Result, WithData):-
    ds_id(Result.entry, Id),
    ds_col_get(entry, Id, [html], Excerpt),
    bc_excerpt(Excerpt.html, 200, Text),
    ds_col_get(user, Result.entry.author,
        [fullname, link], Author),
    Entry = Result.entry.put(_{
        excerpt: Text,
        author: Author
    }),
    WithData = Result.put(entry, Entry).

% Succeeds when the entry has
% the given id.

same_entry(Id, Entry):-
    ds_id(Entry, Id).

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
    Similarity = _{ entry: Entry, score: Cosine }.
