:- module(bc_search, [
    bc_search/4,            % +Type, +Language, +Query, -Results
    bc_index/1,             % +Id
    bc_index_remove/1,      % +Id
    bc_index_remove/0,      % +Id
    bc_index_all/0,
    bc_index_clean/0,
    bc_cosine_similarity/3, % +Id1, +Id2, -Cosine
    bc_add_stopword/1,      % +Word
    bc_terms/1              % -List
]).

/** <module> Search support */

:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(docstore)).
:- use_module(library(sort_dict)).
:- use_module(library(dcg/basics)).
:- use_module(library(porter_stem)).

:- use_module(bc_excerpt).

% Reverse index for content
% tokens.

:- dynamic(content_index/4).
:- dynamic(indexed/1).
:- dynamic(term/1).
:- dynamic(term_idf/2).
:- dynamic(entry_tfidf/4).
:- dynamic(stopword/1).

%! bc_index_clean is det.
%
% Cleans current index data.
% Whole index must be rebuilt
% afterwards.

bc_index_clean:-
    with_mutex(bc_index,
        index_clean_unsafe).

index_clean_unsafe:-
    debug(bc_index, 'cleaning index', []),
    retractall(content_index(_, _, _, _)),
    retractall(indexed(_)),
    retractall(term(_)),
    retractall(term_idf(_, _)),
    retractall(entry_term_tfidf(_, _, _, _)).

bc_terms(List):-
    findall(Idf-Term, term_idf(Term, Idf), Pairs),
    sort(Pairs, List).

bc_add_stopword(Word):-
    must_be(atom, Word),
    (   stopword(Word)
    ->  true
    ;   assertz(stopword(Word))).

:- bc_add_stopword(the).
:- bc_add_stopword(to).
:- bc_add_stopword(for).
:- bc_add_stopword(in).
:- bc_add_stopword(is).
:- bc_add_stopword(it).
:- bc_add_stopword(about).
:- bc_add_stopword(have).
:- bc_add_stopword(many).
:- bc_add_stopword(other).
:- bc_add_stopword(some).
:- bc_add_stopword(that).
:- bc_add_stopword(them).
:- bc_add_stopword(this).
:- bc_add_stopword(when).

% Calculates cosine similarity
% between the two given documents.
% See also:
% https://janav.wordpress.com/2013/10/27/tf-idf-and-cosine-similarity/

bc_cosine_similarity(Id1, Id2, Cosine):-
    must_be(atom, Id1),
    must_be(atom, Id2),
    entry_tfidf(Id1, Vector1, Norm1, NonZero1),
    score_vector(Vector1, Norm1, NonZero1, Id2, Cosine).

score_vector(Vector1, Norm1, NonZero1, Id, Cosine):-
    entry_tfidf(Id, Vector2, Norm2, NonZero2),
    ord_intersection(NonZero1, NonZero2, NonZero),
    dot_product(NonZero, 0, Vector1, Vector2, Product),
    Cosine is Product / (Norm1 * Norm2).

% Calculates dot product between the
% two given documents.

dot_product([Index|Indices], Acc, Vector1, Vector2, Product):-
    arg(Index, Vector1, TfIdf1),
    arg(Index, Vector2, TfIdf2),
    Tmp is TfIdf1 * TfIdf2 + Acc,
    dot_product(Indices, Tmp, Vector1, Vector2, Product).

dot_product([], Acc, _, _, Acc).

term_freq(Id, Term, Freq):-
    (   content_index(Term, Id, _, Freq)
    ->  true
    ;   Freq = 0).

% Rebuilds IDF values for terms.

term_idf_rebuild:-
    debug(bc_search, 'rebuilding IDF factor index', []),
    retractall(term_idf(_, _)),
    findall(Term, term(Term), Terms),
    findall(_, indexed(_), Docs),
    length(Docs, Count),
    maplist(add_term_idf(Count), Terms).

% Calculates and updates the
% IDF value for the given term.

add_term_idf(Count, Term):-
    findall(_, content_index(Term, _, _, _), Docs),
    length(Docs, NumDocs),
    (   NumDocs = 0
    ->  Idf = 1
    ;   Idf is 1 + log(Count/NumDocs)),
    assertz(term_idf(Term, Idf)).

% Rebuilds all TF*IDF vectors.

entry_tfidf_rebuild_all:-
    debug(bc_search, 'rebuilding all TF*IDF vectors', []),
    findall(Id, indexed(Id), Ids),
    maplist(entry_tfidf_rebuild, Ids).

% Rebuilds precalculated entry TF*IDF vector.

entry_tfidf_rebuild(Id):-
    debug(bc_search, 'rebuilding TF*IDF vector for ~w', [Id]),
    retractall(entry_tfidf(Id, _, _, _)),
    findall(Term-Idf, term_idf(Term, Idf), IdfsPairs),
    maplist(entry_term_tfidf(Id), IdfsPairs, Items),
    items_tfidf_vector_norm(Items, Vector, Norm, NonZero),
    assertz(entry_tfidf(Id, Vector, Norm, NonZero)).

items_tfidf_vector_norm(Items, Vector, Norm, NonZero):-
    Vector =.. [tfid|Items],
    findall(Index, (
        arg(Index, Vector, Value), Value > 0),
        NonZero),
    tfidf_vector_norm(NonZero, 0, Vector, Norm).

% Calculates the TF*IDF value for the given
% term in the given entry.

entry_term_tfidf(Id, Term-Idf, TfIdf):-
    term_freq(Id, Term, Freq),
    TfIdf is Freq * Idf.

% Calculates vector norm from the TF*IDF vector.

tfidf_vector_norm([Index|Indices], Acc, Vector, Norm):-
    arg(Index, Vector, TfIdf),
    Tmp is TfIdf * TfIdf + Acc,
    tfidf_vector_norm(Indices, Tmp, Vector, Norm).

tfidf_vector_norm([], Sum, _, Norm):-
    Norm is sqrt(Sum).

%! bc_search(Type, Language, Query, Results) is det.
%
% Runs search against the given type.
% Gives back matching results sorted
% by match score. Each entry is added
% excerpt from beginning.

bc_search(Type, Language, Query, Results):-
    split(Query, Language, Tokens),
    ds_find(entry, (type=Type, published=true),
        [slug, tags, title, author, date_published,
        date_updated, description, language], Entries),
    query_tfidf_norm(Tokens, Vector, Norm, NonZero),
    (   NonZero = []
    ->  Results = []
    ;   maplist(score_entry(Vector, Norm, NonZero), Entries, Scored),
        include(above_zero, Scored, Filtered),
        sort_dict(search_score, desc, Filtered, Sorted),
        maplist(add_excerpt, Sorted, WithExcerpt),
        maplist(add_author, WithExcerpt, Results)).

query_tfidf_norm(Tokens, Vector, Norm, NonZero):-
    findall(Term-Idf, term_idf(Term, Idf), IdfsPairs),
    maplist(query_term_tfidf(Tokens), IdfsPairs, Items),
    items_tfidf_vector_norm(Items, Vector, Norm, NonZero).

% Helper to turn query tokens
% into a vector of TF*IDF values.

query_term_tfidf(Tokens, Term-Idf, TfIdf):-
    (   memberchk(Term, Tokens)
    ->  TfIdf = Idf
    ;   TfIdf = 0).

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

score_entry(Vector, Norm, NonZero, Entry, WithScore):-
    ds_id(Entry, Id),
    score_vector(Vector, Norm, NonZero, Id, Score),
    WithScore = Entry.put(search_score, Score).

%! bc_index_all is det.
%
% Indexes all entries.

bc_index_all:-
    debug(bc_search, 'indexing all entries', []),
    with_mutex(bc_index, (
        ds_all(entry, [], Entries),
        maplist(index_entry, Entries),
        term_idf_rebuild,
        entry_tfidf_rebuild_all)).

index_entry(Entry):-
    ds_id(Entry, Id),
    index_unsafe(Id).

%! bc_index(+Id, +Content) is det.
%
% Indexes the given entry. Should be
% called when the entry contents is
% updated.

bc_index(Id):-
    must_be(atom, Id),
    with_mutex(bc_index, (
        index_unsafe(Id),
        term_idf_rebuild,
        entry_tfidf_rebuild(Id))).

index_unsafe(Id):-
    debug(bc_search, 'indexing ~w', [Id]),
    retractall(content_index(_, Id, _, _)),
    retractall(indexed(Id)),
    ds_col_get(entry, Id,
        [content, tags, title, slug, language], Entry),
    index_content(Id, Entry.content, Entry.language),
    index_tags(Id, Entry.tags, Entry.language),
    index_title(Id, Entry.title, Entry.language),
    index_slug(Id, Entry.slug, Entry.language),
    assertz(indexed(Id)).

index_content(Id, Content, Language):-
    split(Content, Language, Tokens),
    length(Tokens, Length),
    maplist(add_content_token(Id, Length), Tokens).

index_tags(Id, Tags, Language):-
    atomic_list_concat(Tags, ' ', Concat),
    split(Concat, Language, Tokens),
    maplist(add_tag_token(Id), Tokens).

index_title(Id, Title, Language):-
    split(Title, Language, Tokens),
    maplist(add_tag_token(Id), Tokens).

index_slug(Id, Slug, Language):-
    split(Slug, Language, Tokens),
    maplist(add_tag_token(Id), Tokens).

% Adds tag token. Tag token has
% relative weight 1.

add_tag_token(Id, Token):-
    add_term(Token),
    retractall(content_index(Token, Id, _, _)),
    assertz(content_index(Token, Id, 1, 1)).

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

add_content_token(Id, Length, Token):-
    add_term(Token),
    (   content_index(Token, Id, Count, _)
    ->  retractall(content_index(Token, Id, _, _)),
        NewCount is Count + 1,
        NewRel is NewCount / Length,
        assertz(content_index(Token, Id, NewCount, NewRel))
    ;   NewRel is 1/Length,
        assertz(content_index(Token, Id, 1, NewRel))).

% Helper to add token. Only
% adds when it does not exist yet.

add_term(Token):-
    (   term(Token)
    ->  true
    ;   assertz(term(Token))).

% Stems the given term. Non-english entries will not
% have stemmed terms.

stem_term(en, Term, Stemmed):- !,
    catch(porter_stem(Term, Stemmed), Error, true),
    (   var(Error)
    ->  true
    ;   Stemmed = Term).

stem_term(_, Term, Term).

split(Text, Language, Stemmed):-
    atom_codes(Text, Codes),
    split(Codes, [], [], Tokens),
    exclude(unused_token, Tokens, Filtered),
    maplist(stem_term(Language), Filtered, Stemmed).

% Tokens with length < 2 and stopwords are
% not used.

unused_token(Token):-
    atom_length(Token, Length),
    Length < 2, !.

unused_token(Token):-
    stopword(Token).

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

split_at([46,Code|_], _):-
    code_type(Code, digit), !,
    fail.

split_at([Code|_], _):-
    \+ code_type(Code, alnum).