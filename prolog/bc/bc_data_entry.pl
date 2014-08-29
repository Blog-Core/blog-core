:- module(bc_data_entry, [
    bc_entry_save/2,          % +Entry, -Id
    bc_entry_update/2,        % +Id, +Entry
    bc_entry_remove/1,        % +Id
    bc_entry_list/2,          % +Type, -List
    bc_entry/2,               % +Id, -Entry
    bc_entry_info/2           % +Id, -Entry
]).

/** <module> Handles entry data */

:- use_module(library(debug)).
:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).

:- use_module(bc_data_cur_user).

%! bc_entry_save(+Entry, -Id) is det.
%
% Saves and formats the new entry.

bc_entry_save(Entry, Id):-
    check_existing_slug(Entry),
    entry_format(Entry, Formatted),
    bc_user(User),
    put_dict(author, Formatted, User.'$id', Processed),
    ds_insert(Processed, Id),
    debug(bc_data_entry, 'saved entry ~p', [Id]).

%! bc_entry_update(+Id, +Entry) is det.
%
% Updates the given entry. Reformats HTML.

bc_entry_update(Id, Entry):-
    check_post_exists(Id),
    check_existing_slug(Id, Entry),
    check_editing_own_post(Id),
    check_editing_ownership(Entry),
    entry_format(Entry, Formatted),
    put_dict('$id', Formatted, Id, Update),
    ds_update(Update),
    debug(bc_data_entry, 'updated entry ~p', [Id]).

% Formats entry HTML contents based on
% the entries content type.

entry_format(EntryIn, EntryOut):-
    Content = EntryIn.content,
    ContentType = EntryIn.content_type,
    (   ContentType = markdown
    ->  md_html_string(Content, Html)
    ;   Html = Content),
    put_dict(_{ html: Html }, EntryIn, EntryOut).

%! bc_entry_remove(+Id) is det.
%
% Removes the given entry and its comments.

bc_entry_remove(Id):-
    check_post_exists(Id),
    check_editing_own_post(Id),
    ds_remove(Id),
    ds_remove(comment, post=Id),
    debug(bc_data_entry, 'removed entry ~p', [Id]).

%! bc_entry_list(+Type, -List) is det.
%
% Retrieves the list of entries of certain
% type. Does not include contents and HTML.
% Sorts by date_updated desc.

bc_entry_list(Type, Sorted):-
    ds_find(entry, type=Type, [slug, type, date_published,
        date_updated, commenting, published,
        title, author], Entries),
    maplist(attach_comment_count, Entries, List),
    sort_dict(date_updated, desc, List, Sorted),
    debug(bc_data_entry, 'retrieved entry list', []).

%! bc_entry(+Id, -Entry) is det.
%
% Retrieves a single entry by its Id.

bc_entry(Id, WithCount):-
    check_post_exists(Id),
    ds_get(Id, [slug, type, date_published, date_updated,
        commenting, published, title, author,
        content, description, content_type, tags], Entry), !,
    attach_comment_count(Entry, WithCount),
    debug(bc_data_entry, 'retrieved entry ~p', [Id]).

%! bc_entry_info(+Id, -Entry) is det.
%
% Retrieves a single entry by its Id.
% Does not include the content field.

% FIXME maybe it can be removed.

bc_entry_info(Id, WithCount):-
    check_post_exists(Id),
    ds_get(Id, [slug, type, date_published, date_updated,
        commenting, published, title, author,
        description, content_type, tags], Entry),
    attach_comment_count(Entry, WithCount),
    debug(bc_data_entry, 'retrieved entry ~p info', [Id]).

% Attaches comment count to the entry.

attach_comment_count(EntryIn, EntryOut):-
    Id = EntryIn.'$id',
    ds_find(comment, post=Id, [post], List),
    length(List, Count),
    put_dict(_{ comments: Count }, EntryIn, EntryOut).

check_editing_ownership(Update):-
    bc_user(User),
    (   User.type = admin
    ->  true
    ;   (   Update.author = User.'$id'
        ->  true
        ;   throw(error(entry_new_ownership)))).

check_editing_own_post(PostId):-
    bc_user(User),
    (   User.type = admin
    ->  true
    ;   ds_get(PostId, [author], Original),
        (   Original.author = User.'$id'
        ->  true
        ;   throw(error(entry_is_not_own)))).

% FIXME use better method.

check_post_exists(PostId):-
    (   ds_get(PostId, [slug], _)
    ->  true
    ;   throw(error(entry_not_exists))).

check_existing_slug(Entry):-
    (   ds_find(entry, slug=Entry.slug, [])
    ->  true
    ;   throw(error(entry_existing_slug))).

check_existing_slug(PostId, Update):-
    (   ds_find(entry, slug=Update.slug, [Post])
    ->  (   Post.'$id' = PostId
        ->  true
        ;   throw(error(entry_existing_slug)))
    ;   true).
