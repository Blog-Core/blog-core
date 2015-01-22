:- module(bc_entry, [
    bc_entry_exists/1,     % +Id
    bc_entry_author/2,     % +Id, -AuthorId
    bc_entry_type/2,       % +Id, -Type
    bc_entry_published/2,  % +Id, -Published
    bc_entry_commenting/2, % +Id, -Commenting
    bc_slug_id/2,          % +Slug, -Id
    bc_slug_unique/1,      % +Slug
    bc_slug_unique/2       % +Slug, +Id
]).

:- use_module(library(docstore)).

% FIXME document

bc_entry_author(Id, AuthorId):-
    ds_get(Id, [author], Entry),
    Entry.author = AuthorId.

bc_entry_type(Id, Type):-
    ds_get(Id, [type], Entry),
    Entry.type = Type.

bc_entry_published(Id, Published):-
    ds_get(Id, [published], Entry),
    Entry.published = Published.

bc_entry_commenting(Id, Commenting):-
    ds_get(Id, [commenting], Entry),
    Entry.commenting = Commenting.

% Finds entry id by slug.

bc_slug_id(Slug, Id):-
    ds_find(entry, slug=Slug, [slug], [Entry]),
    Id = Entry.'$id'.

% Checks that slug is not used before.

bc_slug_unique(Slug):-
    \+ bc_slug_id(Slug, _), !.

bc_slug_unique(_):-
    throw(error(existing_slug)).

% Checks that slug is not used
% for another post.

bc_slug_unique(Slug, _):-
    \+ bc_slug_id(Slug, _), !.

bc_slug_unique(Slug, Id):-
    bc_slug_id(Slug, Id), !.

bc_slug_unique(_, _):-
    throw(error(existing_slug)).

% Checks that the given
% entry exists.

bc_entry_exists(Id):-
    bc_entry_type(Id, _), !.

bc_entry_exists(_):-
    throw(error(entry_not_exists)).
