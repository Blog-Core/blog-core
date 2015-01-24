:- module(bc_data_user, [
    bc_user_auth/2,         % +Auth, -Info
    bc_user_save/3,         % +Actor, +User, -Id
    bc_user_save_initial/1, % +User
    bc_user_update/2,       % +Actor, +User
    bc_user_remove/2,       % +Actor, +Id
    bc_user_list/2,         % +Actor, -Users
    bc_user/3               % +Actor, +Id, -User
]).

/** <module> Handles the user and authentication data */

:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(sha)).

:- use_module(bc_user).
:- use_module(bc_access).
:- use_module(bc_role).

%! bc_user_auth(+Auth, -Info) is det.
%
% Authenticates the given user with
% username and password.

bc_user_auth(Auth, Info):-
    user_auth(Auth, User),
    login_access(User),
    Info = _{
        id: User.'$id',
        type: User.type,
        key: User.key },
    Username = Auth.username,
    debug(bc_data, 'authenticated user ~p', [Username]).

login_access(User):-
    bc_login_access(User), !.

login_access(_):-
    throw(error(no_login_access)).

% Attempts to identify and authenticate
% the user based on supplied credentials.

user_auth(Auth, User):-
    ds_find(user, username=Auth.username, [User]),
    password_hash(Auth.password, User.salt, User.password), !.

user_auth(_, _):-
    throw(error(invalid_credentials)).

%! bc_user_save(+Actor, +User, -Id) is det.
%
% Saves the new user.

bc_user_save(Actor, User, Id):-
    users_access(Actor),
    user_save_common(User, Id).

users_access(Actor):-
    Actor.type = admin, !.

users_access(_):-
    throw(error(no_access)).

%! bc_user_save_initial(+User, -Id) is det.
%
% Same as bc_user_save/2 but does not run check
% against the current user. Used for populating
% the initial database.

bc_user_save_initial(User):-
    user_save_common(User, _).

% Saves the new user. Used by
% wrapper predicates bc_user_save/2 and
% bc_user_save_initial/2.

user_save_common(User, Id):-
    bc_valid_username(User.username),
    bc_unique_username(User.username),
    bc_valid_role(User.type),
    user_hash(User, Hashed),
    ds_uuid(Key),
    put_dict(key, Hashed, Key, Keyed),
    ds_insert(Keyed, Id),
    debug(bc_data, 'saved user ~p', [Id]).

%! bc_user_update(+Actor, +Id, +User) is det.
%
% Updates the given user.

bc_user_update(Actor, User):-
    Id = User.'$id',
    users_access(Actor),
    bc_user_exists(Id),
    bc_valid_username(User.username),
    bc_unique_username(User.username, Id),
    bc_valid_role(User.type),
    (   User.type = admin
    ->  true
    ;   bc_remaining_admin(Id)),
    user_hash(User, Hashed),
    ds_update(Hashed),
    debug(bc_data, 'updated user ~p', [Id]).

% (Re)hashes the user password when password
% is set in the user dict. Replaces the password
% in user dict with salted hash. Uses a freshly
% generated UUID as salt.

user_hash(UserIn, UserOut):-
    (   get_dict(password, UserIn, Password)
    ->  ds_uuid(Salt),
        password_hash(Password, Salt, Hash),
        put_dict(_{ password: Hash, salt: Salt }, UserIn, UserOut)
    ;   UserOut = UserIn).

%! bc_user_list(+Actor, -Sorted) is det.
%
% Retrieves the list of users. Retrieved
% fields are `username`, `fullname` and `type`.

bc_user_list(Actor, Sorted):-
    users_access(Actor),
    ds_all(user, [username, fullname, type], Users),
    sort_dict(username, asc, Users, Sorted),
    debug(bc_data, 'retrieved the users list', []).

%! bc_user(+Actor, +Id, -User) is det.
%
% Retrieves the given user. Retrieved
% fields are `username`, `fullname`, `type`, `link`.

bc_user(Actor, Id, User):-
    users_access(Actor),
    bc_user_exists(Id),
    ds_get(Id, [username, fullname, type, link], User),
    debug(bc_data, 'retrieved the user ~p', [Id]).

%! bc_user_remove(+Actor, +Id) is det.
%
% Removes the given user.

bc_user_remove(Actor, Id):-
    users_access(Actor),
    bc_user_exists(Id),
    bc_remaining_admin(Id),
    bc_no_entries(Id),
    ds_remove(Id),
    debug(bc_data, 'removed user ~p', [Id]).

% Produces hex-formatted hash from
% password and salt.

password_hash(Password, Salt, Hash):-
    atom_concat(Salt, Password, Data),
    sha_hash(Data, Raw, [encoding(utf8), algorithm(sha256)]),
    hash_atom(Raw, Hash).
