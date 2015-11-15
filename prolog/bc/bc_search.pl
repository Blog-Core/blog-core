:- module(bc_search, [
    bc_search/3,
    bc_index/1,        % +Id
    bc_index_remove/1, % +Id
    bc_index_remove/0, % +Id
    bc_index_all/0,
    bc_index_size/1    % -Bytes
]).

/** <module> Search support */

:- use_module(library(debug)).
:- use_module(library(docstore)).
:- use_module(library(sort_dict)).
:- use_module(library(dcg/basics)).

:- use_module(bc_excerpt).

% Reverse index for content
% tokens.

:- dynamic(content_index/4).

%! bc_search(Type, Query, Results) is det.
%
% Runs search against the given type.
% Gives back matching results sorted
% by match score. Each entry is added
% excerpt from beginning.

bc_search(Type, Query, Results):-
    split(Query, Tokens),
    ds_find(entry, (type=Type, published=true),
        [slug, tags, title, author, date_published,
        date_updated, description, language], Entries),
    maplist(score_tokens(Tokens), Entries, Scored),
    include(above_zero, Scored, Filtered),
    sort_dict(search_score, desc, Filtered, Sorted),
    maplist(add_excerpt, Sorted, WithExcerpt),
    maplist(add_author, WithExcerpt, Results).

above_zero(Entry):-
    Entry.search_score > 0.

add_excerpt(Entry, WithExcerpt):-
    ds_id(Entry, Id),
    ds_col_get(entry, Id, [html], Excerpt),
    bc_excerpt(Excerpt.html, 200, Text),
    WithExcerpt = Entry.put(excerpt, Text).

add_author(Entry, WithAuthor):-
    ds_col_get(user, Entry.author, [fullname, link], Author),
    WithAuthor = Entry.put(author, Author).

% Attaches search_score key to entry
% based on token relevancy. Sets search_score
% to -1 when no token matches.

score_tokens(Tokens, Entry, WithScore):-
    ds_id(Entry, Id),
    maplist(score_token(Id), Tokens, Scores),
    (   memberchk(-1, Scores)
    ->  Total = -1
    ;   sum_list(Scores, Total)),
    WithScore = Entry.put(search_score, Total).

score_token(Id, Token, Score):-
    (   content_index(Token, Id, _, Score)
    ->  true
    ;   Score = -1).

%! bc_index_size(-Bytes) is det.
%
% Gives the memory usage of index
% by number of bytes. This does include
% atoms held by the index.

bc_index_size(Bytes):-
    findall(Ref,
        nth_clause(content_index(_, _, _, _), _, Ref), List),
    maplist(clause_size, List, Sizes),
    sum_list(Sizes, Bytes).

clause_size(Ref, Size):-
    clause_property(Ref, size(Size)).

%! bc_index_all is det.
%
% Indexes all entries.

bc_index_all:-
    debug(bc_search, 'indexing all entries', []),
    ds_all(entry, [], Entries),
    maplist(index_entry, Entries).

index_entry(Entry):-
    ds_id(Entry, Id),
    bc_index(Id).

%! bc_index(+Id, +Content) is det.
%
% Indexes the given entry. Should be
% called when the entry contents is
% updated.

bc_index(Id):-
    must_be(atom, Id),
    with_mutex(bc_index,
        bc_index_unsafe(Id)).

bc_index_unsafe(Id):-
    debug(bc_search, 'indexing ~w', [Id]),
    retractall(content_index(_, Id, _, _)),
    ds_col_get(entry, Id,
        [content, tags, title, slug], Entry),
    split(Entry.content, Tokens),
    length(Tokens, Length),
    maplist(add_token(Id, Length), Tokens),
    maplist(add_tag_token(Id), Entry.tags),
    split(Entry.title, TitleTokens),
    maplist(add_tag_token(Id), TitleTokens),
    split(Entry.slug, SlugTokens),
    maplist(add_tag_token(Id), SlugTokens).

% Adds tag token. Tag token has
% relative weight 1.

add_tag_token(Id, Tag):-
    retractall(content_index(Tag, Id, _, _)),
    assertz(content_index(Tag, Id, 1, 1)).

%! bc_index_remove(+Id) is det.
%
% Removes the given entry
% index.

bc_index_remove(Id):-
    must_be(atom, Id),
    with_mutex(bc_index,
        retractall(content_index(_, Id, _, _))).

%! bc_index_remove is det.
%
% Removes all indexes.

bc_index_remove:-
    with_mutex(bc_index,
        retractall(content_index(_, _, _, _))).

% Adds token to the index.
% Recalculates relative historgram.

add_token(Id, Length, Token):-
    (   content_index(Token, Id, Count, _)
    ->  retractall(content_index(Token, Id, _, _)),
        NewCount is Count + 1,
        NewRel is NewCount / Length,
        assertz(content_index(Token, Id, NewCount, NewRel))
    ;   NewRel is 1/Length,
        assertz(content_index(Token, Id, 1, NewRel))).

split(Text, Filtered):-
    atom_codes(Text, Codes),
    split(Codes, [], [], Tokens),
    exclude(empty_token, Tokens, Filtered).

empty_token(Token):-
    atom_length(Token, Length),
    Length < 2.

% Splits list of codes into a list
% of tokens (atoms).

split([Code|Codes], Context, Acc, Tokens):-
    (   split_at([Code|Codes], Context)
    ->  reverse(Acc, Token),
        atom_codes(Atom, Token),
        downcase_atom(Atom, Lower),
        Tokens = [Lower|Rest],
        split(Codes, [Code|Context], [], Rest)
    ;   split(Codes, [Code|Context], [Code|Acc], Tokens)).

split([], _, Acc, [Lower]):-
    reverse(Acc, Token),
    atom_codes(Atom, Token),
    downcase_atom(Atom, Lower).

% Whether to split at current code or not.

% Preserves inter-word dot.

split_at([0'.,Code|_], _):-
    code_type(Code, digit), !,
    fail.

split_at([Code|_], _):-
    \+ code_type(Code, alnum).