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
% Throws error(existing_slug(Slug))
% when the entry slug already exists.

bc_entry_save(Entry, Id):-
    get_dict_ex(slug, Entry, Slug),
    ds_find(entry, slug=Slug, Existing),
    length(Existing, Length),
    (   Length > 0
    ->  throw(error(existing_slug(Slug)))
    ;   bc_entry_format(Entry, Formatted),
        bc_user(User),
        get_dict_ex('$id', User, UserId),
        put_dict(author, Formatted, UserId, Processed),
        ds_insert(Processed, Id),
        debug(bc_data_entry, 'saved entry ~p', [Id])).

%! bc_entry_update(+Id, +Entry) is det.
%
% Updates the given entry. Reformats HTML.

bc_entry_update(Id, Entry):-
    bc_entry_format(Entry, Formatted),
    put_dict('$id', Formatted, Id, Update),
    ds_update(Update),
    debug(bc_data_entry, 'updated entry ~p', [Id]).

% Formats entry HTML contents based on
% the entries content type.

bc_entry_format(EntryIn, EntryOut):-
    get_dict_ex(content, EntryIn, Content),
    get_dict_ex(content_type, EntryIn, ContentType),
    (   ContentType = markdown
    ->  md_html_string(Content, Html)
    ;   Html = Content),
    put_dict(_{ html: Html }, EntryIn, EntryOut).

%! bc_entry_remove(+Id) is det.
%
% Removes the given entry and its comments.

bc_entry_remove(Id):-
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
    sort_dict(date_updated, desc, List, Sorted).

attach_comment_count(EntryIn, EntryOut):-
    get_dict_ex('$id', EntryIn, Id),
    ds_find(comment, post=Id, [post], List),
    length(List, Count),
    put_dict(_{ comments: Count }, EntryIn, EntryOut).

%! bc_entry(+Id, -Entry) is det.
%
% Retrieves a single entry by ID. Throws
% error(no_entry(Id)) when there is no such entry.

bc_entry(Id, WithCount):-
    (   ds_get(Id, [slug, type, date_published, date_updated,
            commenting, published, title, author,
            content, description, content_type, tags], Entry),
        attach_comment_count(Entry, WithCount)
    ;   throw(error(no_entry(Id)))), !.

%! bc_entry_info(+Id, -Entry) is det.
%
% Retrieves a single entry by ID. Throws
% error(no_entry(Id)) when there is no such entry.
% Does not include content.

bc_entry_info(Id, WithCount):-
    (   ds_get(Id, [slug, type, date_published, date_updated,
            commenting, published, title, author,
            description, content_type, tags], Entry),
        attach_comment_count(Entry, WithCount)
    ;   throw(error(no_entry(Id)))), !.
