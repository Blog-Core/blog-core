:- module(bc_data_entry, [
    bc_entry_save/3,          % +Actor, +Entry, -Id
    bc_entry_update/2,        % +Actor, +Entry
    bc_entry_remove/2,        % +Actor, +Id
    bc_entry_remove_trash/2,  % +Actor, +Id
    bc_entry_restore/2,       % +Actor, +Id
    bc_entry_list/3,          % +Actor, +Type, -List
    bc_trash_list/2,          % +Actor, -List
    bc_purge_trash/1,         % +Actor
    bc_entry/3,               % +Actor, +Id, -Entry
    bc_entry_info/3           % +Actor, +Id, -Entry
]).

/** <module> Handles entry data */

:- use_module(library(debug)).
:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).

:- use_module(bc_access).
:- use_module(bc_search).
:- use_module(bc_entry).
:- use_module(bc_files).
:- use_module(bc_user).

%! bc_entry_save(+Actor, +Entry, -Id) is det.
%
% Saves and formats the new entry.

bc_entry_save(Actor, Entry, Id):-
    can_create(Actor, Entry),
    entry_format(Entry, Formatted),
    ds_insert(Formatted, Id),
    bc_index(Id),
    debug(bc_data_entry, 'saved entry ~p', [Id]).

can_create(Actor, Entry):-
    bc_valid_slug(Entry.slug),
    bc_slug_unique(Entry.slug),
    bc_user_exists(Entry.author),
    create_access(Actor, Entry).

create_access(Actor, Entry):-
    bc_create_access_type(Actor, Entry.type), !.

create_access(_, _):-
    throw(error(no_access)).

%! bc_entry_update(+Actor, +Entry) is det.
%
% Updates the given entry. Reformats HTML.

bc_entry_update(Actor, Entry):-
    ds_id(Entry, Id),
    can_update(Actor, Entry),
    bc_entry_slug(Id, OldSlug),
    entry_format(Entry, OldSlug, Formatted),
    ds_update(Formatted),
    bc_index(Id),
    rename_directory(OldSlug, Entry.slug),
    debug(bc_data_entry, 'updated entry ~p', [Id]).

can_update(Actor, Entry):-
    ds_id(Entry, Id),
    bc_entry_exists(Id),
    bc_valid_slug(Entry.slug),
    bc_slug_unique(Entry.slug, Id),
    bc_user_exists(Entry.author),
    update_access(Actor, Entry).

update_access(Actor, Entry):-
    ds_id(Entry, Id),
    update_type_access(Actor, Entry),
    update_author_access(Actor, Entry),
    bc_update_access_id(Actor, Id),
    bc_entry_published(Id, Published),
    (   Entry.published = Published
    ->  true
    ;   bc_publish_access_id(Actor, Id)), !.

update_access(_, _):-
    throw(error(no_access)).

% Checks if the entry
% type can be updated.

update_type_access(Actor, _):-
    Actor.type = admin, !.

update_type_access(_, Entry):-
    ds_id(Entry, Id),
    bc_entry_type(Id, Entry.type), !.

update_type_access(_, _):-
    throw(error(no_access)).

% Checks if the entry
% author can be updated.

update_author_access(Actor, _):-
    Actor.type = admin, !.

update_author_access(_, Entry):-
    ds_id(Entry, Id),
    bc_entry_author(Id, Entry.author), !.

update_author_access(_, _):-
    throw(error(no_access)).

% Renames entry files directory
% when the entry slug changes.

rename_directory(Slug, Slug):- !.

rename_directory(Old, New):-
    atomic_list_concat([public, '/', Old], From),
    atomic_list_concat([public, '/', New], To),
    (   exists_directory(From)
    ->  rename_file(From, To)
    ;   true).

% Formats entry HTML contents based on
% the entries content type.

entry_format(EntryIn, OldSlug, EntryOut):-
    links_rewrite(EntryIn.content,
        EntryIn.slug, OldSlug, Content),
    Rewritten = EntryIn.put(content, Content),
    entry_format(Rewritten, EntryOut).

% Replaces slug in links in the content.

links_rewrite(ContentIn, NewSlug, OldSlug, ContentOut):-
    atomic_list_concat(['/', OldSlug, '/'], OldLink),
    atomic_list_concat(['/', NewSlug, '/'], NewLink),
    atomic_list_concat(Tokens, OldLink, ContentIn),
    atomic_list_concat(Tokens, NewLink, ContentAtom),
    atom_string(ContentAtom, ContentOut).

entry_format(EntryIn, EntryOut):-
    Content = EntryIn.content,
    ContentType = EntryIn.content_type,
    (   ContentType = markdown
    ->  md_html_string(Content, Html)
    ;   Html = Content),
    put_dict(_{ html: Html }, EntryIn, EntryOut).

%! bc_entry_remove(+Actor, +Id) is det.
%
% Removes the given entry and its comments.

bc_entry_remove(Actor, Id):-
    can_remove(Actor, Id),
    ds_move(entry, Id, trash),
    debug(bc_data_entry, 'moved to trash: ~p', [Id]).

%! bc_entry_remove_trash(+Actor, +Id) is det.
%
% Removes entry from trash.

bc_entry_remove_trash(Actor, Id):-
    can_remove(Actor, Id),
    bc_entry_slug(Id, Slug),
    ds_col_remove(trash, Id),
    ds_col_remove_cond(comment, post=Id),
    bc_index_remove(Id),
    remove_files(Slug),
    debug(bc_data_entry, 'removed entry ~p', [Id]).

%! bc_entry_restore(+Actor, +Id) is det.
%
% Restores the given entry from trash.
% Require remove permission.

bc_entry_restore(Actor, Id):-
    can_remove(Actor, Id),
    ds_move(trash, Id, entry).

can_remove(Actor, Id):-
    bc_entry_exists(Id),
    remove_access(Actor, Id).

remove_access(Actor, Id):-
    bc_remove_access_id(Actor, Id), !.

remove_access(_, _):-
    throw(error(no_access)).

% Removes entry files.

remove_files(Slug):-
    atomic_list_concat([public, '/', Slug], Directory),
    (   exists_directory(Directory)
    ->  remove_directory(Directory)
    ;   true).

%! bc_entry_list(+Actor, +Type, -List) is det.
%
% Retrieves the list of entries of certain
% type. Does not include contents and HTML.
% Sorts by date_updated desc.

bc_entry_list(Actor, Type, Sorted):-
    ds_find(entry, type=Type, [slug, type, date_published,
        date_updated, commenting, published,
        title, author, tags], Entries),
    include(bc_read_access_entry(Actor), Entries, Filtered),
    maplist(attach_comment_count, Filtered, List),
    sort_dict(date_updated, desc, List, Sorted),
    debug(bc_data_entry, 'retrieved entry list', []).

%! bc_trash_list(+Actor, -List) is det.
%
% Retrieves the list of entries in trash.
% Does not include contents and HTML.
% Sorts by date_updated desc. Only includes
% these that the user has access to.

bc_trash_list(Actor, Sorted):-
    ds_all(trash, [slug, type, date_published,
        date_updated, commenting, published,
        title, author, tags], Entries),
    include(bc_remove_access_entry(Actor), Entries, Filtered),
    maplist(attach_comment_count, Filtered, List),
    sort_dict(date_updated, desc, List, Sorted),
    debug(bc_data_entry, 'retrieved trash list', []).

%! bc_purge_trash(Actor) is det.
%
% Removes all entries from trash
% that the actor has access to.

bc_purge_trash(Actor):-
    ds_all(trash, [type, author], Entries),
    include(bc_remove_access_entry(Actor), Entries, Filtered),
    maplist(ds_id, Filtered, Ids),
    maplist(bc_entry_remove_trash(Actor), Ids),
    debug(bc_data_entry, 'purged trash', []).

%! bc_entry(+Actor, +Id, -Entry) is det.
%
% Retrieves a single entry by its Id.

bc_entry(Actor, Id, WithCount):-
    can_view(Actor, Id),
    ds_col_get(entry, Id, [slug, type, date_published, date_updated,
        commenting, published, title, author,
        content, description, content_type, tags, language], Entry), !,
    attach_comment_count(Entry, WithCount),
    debug(bc_data_entry, 'retrieved entry ~p', [Id]).

%! bc_entry_info(+Actor, +Id, -Entry) is det.
%
% Retrieves a single entry by its Id.
% Does not include the content field.

bc_entry_info(Actor, Id, WithCount):-
    can_view(Actor, Id),
    ds_col_get(entry, Id, [slug, type, date_published, date_updated,
        commenting, published, title, author,
        description, content_type, tags, language], Entry), !,
    attach_comment_count(Entry, WithCount),
    debug(bc_data_entry, 'retrieved entry ~p info', [Id]).

can_view(Actor, Id):-
    bc_entry_exists(Id),
    view_access(Actor, Id).

view_access(Actor, Id):-
    bc_read_access_id(Actor, Id), !.

view_access(_, _):-
    throw(error(no_access)).

% Attaches comment count to the entry.

attach_comment_count(EntryIn, EntryOut):-
    ds_id(EntryIn, Id),
    ds_find(comment, post=Id, [post], List),
    length(List, Count),
    put_dict(_{ comments: Count }, EntryIn, EntryOut).
