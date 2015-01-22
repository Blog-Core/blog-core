:- module(bc_access, [
    bc_type_access/3,       % +Actor, +Action, +Type,
    bc_type_access_by_id/3, % +Actor, +Action, +Id,
    bc_ownership/2,         % +Actor, +AuthorId,
    bc_ownership_by_id/2,   % +Actor, +Id,
    bc_entry_exists/1,      % +Id
    bc_files_access/1       % +Actor
]).

:- use_module(bc_data_type).
:- use_module(bc_data_role).
:- use_module(bc_entry).

% FIXME document.

% Checks that actor has access
% to given entry type for the
% given action.

bc_type_access(Actor, _, _):-
    Actor.type = admin, !.

bc_type_access(Actor, Action, Type):-
    bc_type(Type, _, _, Roles, _),
    member(Role, Roles),
    Role =.. [Name|Grants],
    Actor.type = Name,
    member(Action, Grants), !.

bc_type_access(_, _, _):-
    throw(error(no_type_access)).

bc_type_access_by_id(Actor, Action, Id):-
    bc_entry_type(Id, Type),
    bc_type_access(Actor, Action, Type).

bc_ownership_by_id(Actor, Id):-
    bc_entry_author(Id, AuthorId),
    bc_ownership(Actor, AuthorId).

bc_ownership(Actor, _):-
    Actor.type = admin, !.

bc_ownership(Actor, AuthorId):-
    Actor.'$id' = AuthorId, !.

bc_ownership(_, _):-
    throw(error(no_ownership)).

bc_entry_exists(Id):-
    bc_entry_type(Id, _), !.

bc_entry_exists(_):-
    throw(error(entry_not_exists)).

bc_files_access(Actor):-
    Actor.type = admin, !.

bc_files_access(Actor):-
    Role = Actor.type,
    bc_role(Role, _, _, true), !.

bc_files_access(_):-
    throw(error(no_files_access)).
