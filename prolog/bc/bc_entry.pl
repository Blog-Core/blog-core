:- module(bc_entry, [
    bc_entry_author/2,    % +Id, -AuthorId
    bc_entry_type/2,      % +Id, -Type
    bc_entry_published/2, % +Id, -Published
    bc_slug_id/2          % +Slug, -Id
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

% Finds entry id by slug.

bc_slug_id(Slug, Id):-
    ds_find(entry, slug=Slug, [slug], [Entry]),
    Id = Entry.'$id'.
