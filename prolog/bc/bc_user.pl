:- module(bc_user, [
    bc_valid_username/1,  % +Username
    bc_unique_username/1, % +Username
    bc_unique_username/2, % +Username, +Id
    bc_user_exists/1,     % +Id
    bc_remaining_admin/1, % +Id
    bc_no_entries/1,      % +Id
    bc_valid_role/1       % +Name
]).

:- use_module(library(dcg/basics)).
:- use_module(library(docstore)).

:- use_module(bc_role).

% Checks that the user's username
% is an email address.

bc_valid_username(Username):-
    atom_codes(Username, Codes),
    phrase(email, Codes, []), !.

bc_valid_username(_):-
    throw(error(invalid_username)).

email -->
    string_without(`@`, Start), "@", string_without(`@`, End),
    {   length(Start, LenStart), LenStart > 0,
        length(End, LenEnd), LenEnd > 0 }.

% Checks that username is
% not used before.

bc_unique_username(Username):-
    \+ bc_username_id(Username, _), !.

bc_unique_username(_):-
    throw(error(existing_username)).

% Checks that username is
% not used by other users.

bc_unique_username(Username, _):-
    \+ bc_username_id(Username, _), !.

bc_unique_username(Username, Id):-
    bc_username_id(Username, Id), !.

bc_unique_username(_, _):-
    throw(error(existing_username)).

% Checks that the user's role
% is valid.

bc_valid_role(Name):-
    bc_role(Name, _, _), !.

bc_valid_role(_):-
    throw(error(invalid_role)).

% Checks that there is
% a remaining admin after
% the user is not admin anymore.

bc_remaining_admin(Id):-
    ds_find(user, type=admin, [type], Users),
    member(User, Users),
    User.'$id' \= Id, !.

bc_remaining_admin(_):-
    throw(error(no_remaining_admin)).

% Checks that the user
% has no entries.

bc_no_entries(Id):-
    ds_find(entry, author=Id, [author], Entries),
    length(Entries, 0), !.

bc_no_entries(_):-
    throw(error(has_entries)).

% Checks that the user exists.

bc_user_exists(Id):-
    ds_get(Id, [type], _), !.

bc_user_exists(_):-
    throw(error(user_not_exists)).

% Finds user id by username.

bc_username_id(Username, Id):-
    ds_find(user, username=Username, [username], [User]),
    Id = User.'$id'.
