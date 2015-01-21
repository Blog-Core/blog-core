:- module(bc_data_entry, [
    bc_entry_save/3,          % +Actor, +Entry, -Id
    bc_entry_update/2,        % +Actor, +Entry
    bc_entry_remove/2,        % +Actor, +Id
    bc_entry_list/3,          % +Actor, +Type, -List
    bc_entry/3,               % +Actor, +Id, -Entry
    bc_entry_info/3           % +Actor, +Id, -Entry
]).

/** <module> Handles entry data */

:- use_module(library(debug)).
:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).

:- use_module(bc_access).
:- use_module(bc_entry).

%! bc_entry_save(+Actor, +Entry, -Id) is det.
%
% Saves and formats the new entry.

bc_entry_save(Actor, Entry, Id):-
    can_save(Actor, Entry),
    entry_format(Entry, Formatted),
    ds_insert(Formatted, Id),
    debug(bc_data_entry, 'saved entry ~p', [Id]).

can_save(Actor, Entry):-
    bc_type_access(Actor, Entry.type),
    slug_unique(Entry.slug).

%! bc_entry_update(+Actor, +Entry) is det.
%
% Updates the given entry. Reformats HTML.

bc_entry_update(Actor, Entry):-
    can_update(Actor, Entry),
    entry_format(Entry, Formatted),
    ds_update(Formatted),
    debug(bc_data_entry, 'updated entry ~p', [Entry.'$id']).

can_update(Actor, Entry):-
    bc_entry_exists(Entry.'$id'),
    bc_type_access(Actor, Entry.type),
    bc_type_access_by_id(Actor, Entry.'$id'),
    bc_ownership(Actor, Entry.author),
    bc_ownership_by_id(Actor, Entry.'$id'),
    slug_unique(Entry.slug, Entry.'$id').

% Formats entry HTML contents based on
% the entries content type.

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
    ds_remove(Id),
    ds_remove(comment, post=Id),
    remove_files(Id),
    debug(bc_data_entry, 'removed entry ~p', [Id]).

can_remove(Actor, Id):-
    bc_entry_exists(Id),
    bc_type_access_by_id(Actor, Id),
    bc_ownership_by_id(Actor, Id).

% Removes entry files.

remove_files(Id):-
    atomic_list_concat([public, '/', Id], Directory),
    (   exists_directory(Directory)
    ->  remove_directory(Directory)
    ;   true).

remove_directory(Directory):-
    directory_files(Directory, Entries),
    exclude(ignored_file, Entries, Files),
    maplist(join_directory(Directory), Files, Joined),
    maplist(remove_directory_entry, Joined),
    delete_directory(Directory).

ignored_file('.').
ignored_file('..').

join_directory(Directory, Entry, Joined):-
    atomic_list_concat([Directory, '/', Entry], Joined).

remove_directory_entry(Entry):-
    (   exists_directory(Entry)
    ->  remove_directory(Entry)
    ;   delete_file(Entry)).

%! bc_entry_list(+Actor, +Type, -List) is det.
%
% Retrieves the list of entries of certain
% type. Does not include contents and HTML.
% Sorts by date_updated desc.

bc_entry_list(Actor, Type, Sorted):-
    can_list(Actor, Type),
    ds_find(entry, type=Type, [slug, type, date_published,
        date_updated, commenting, published,
        title, author], Entries),
    maplist(attach_comment_count, Entries, List),
    sort_dict(date_updated, desc, List, Sorted),
    debug(bc_data_entry, 'retrieved entry list', []).

can_list(Actor, Type):-
    bc_type_access(Actor, Type).

%! bc_entry(+Actor, +Id, -Entry) is det.
%
% Retrieves a single entry by its Id.

bc_entry(Actor, Id, WithCount):-
    can_view(Actor, Id),
    ds_get(Id, [slug, type, date_published, date_updated,
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
    ds_get(Id, [slug, type, date_published, date_updated,
        commenting, published, title, author,
        description, content_type, tags, language], Entry), !,
    attach_comment_count(Entry, WithCount),
    debug(bc_data_entry, 'retrieved entry ~p info', [Id]).

can_view(Actor, Id):-
    bc_entry_exists(Id),
    bc_type_access_by_id(Actor, Id).

% Attaches comment count to the entry.

attach_comment_count(EntryIn, EntryOut):-
    Id = EntryIn.'$id',
    ds_find(comment, post=Id, [post], List),
    length(List, Count),
    put_dict(_{ comments: Count }, EntryIn, EntryOut).

% Checks that slug is not used before.

slug_unique(Slug):-
    \+ bc_slug_id(Slug, _), !.

slug_unique(_):-
    throw(error(existing_slug)).

% Checks that slug is not used for another post.

slug_unique(Slug, _):-
    \+ bc_slug_id(Slug, _), !.

slug_unique(Slug, Id):-
    bc_slug_id(Slug, Id), !.

slug_unique(_, _):-
    throw(error(existing_slug)).
