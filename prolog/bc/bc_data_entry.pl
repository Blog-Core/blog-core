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
:- use_module(bc_user).

%! bc_entry_save(+Actor, +Entry, -Id) is det.
%
% Saves and formats the new entry.

bc_entry_save(Actor, Entry, Id):-
    can_create(Actor, Entry),
    entry_format(Entry, Formatted),
    ds_insert(Formatted, Id),
    debug(bc_data_entry, 'saved entry ~p', [Id]).

can_create(Actor, Entry):-
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
    can_update(Actor, Entry),
    entry_format(Entry, Formatted),
    ds_update(Formatted),
    debug(bc_data_entry, 'updated entry ~p', [Entry.'$id']).

can_update(Actor, Entry):-
    Id = Entry.'$id',
    bc_entry_exists(Id),
    bc_slug_unique(Entry.slug, Id),
    bc_user_exists(Entry.author),
    update_access(Actor, Entry).

update_access(Actor, Entry):-
    Id = Entry.'$id',
    bc_entry_type(Id, Old),
    (   Old = Entry.type
    ->  bc_update_access_id(Actor, Id)
    ;   bc_create_access_type(Actor, Entry.type),
        bc_remove_access_id(Actor, Id)),
    (   Entry.published = true
    ->  true
    ;   bc_publish_access(Actor, Entry.type, Id)), !.

update_access(_, _):-
    throw(error(no_access)).

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
    remove_access(Actor, Id).

remove_access(Actor, Id):-
    bc_remove_access_id(Actor, Id), !.

remove_access(_, _):-
    throw(error(no_access)).

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
    ds_find(entry, type=Type, [slug, type, date_published,
        date_updated, commenting, published,
        title, author], Entries),
    include(bc_read_access_entry(Actor), Entries, Filtered),
    maplist(attach_comment_count, Filtered, List),
    sort_dict(date_updated, desc, List, Sorted),
    debug(bc_data_entry, 'retrieved entry list', []).

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
    view_access(Actor, Id).

view_access(Actor, Id):-
    bc_read_access_id(Actor, Id), !.

view_access(_, _):-
    throw(error(no_access)).

% Attaches comment count to the entry.

attach_comment_count(EntryIn, EntryOut):-
    Id = EntryIn.'$id',
    ds_find(comment, post=Id, [post], List),
    length(List, Count),
    put_dict(_{ comments: Count }, EntryIn, EntryOut).
