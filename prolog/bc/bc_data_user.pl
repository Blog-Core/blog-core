:- module(bc_data_user, [
    bc_user_save/2,         % +User, -Id
    bc_user_save_initial/1, % +User
    bc_user_update/2,       % +Id, +User
    bc_user_remove/1,       % +Id
    bc_user_auth/2,         % +Auth, -Info
    bc_user_list/1,         % -Users
    bc_user/2               % +Id, -User
]).

/** <module> Handles the user and authentication data
*/

:- use_module(library(dcg/basics)).
:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(sha)).

:- use_module(bc_data_cur_user).
:- use_module(bc_data_validate).

%! bc_user_auth(+Auth, -Info) is semidet.
%
% Authenticates the given user with
% username and password.

bc_user_auth(Auth, Info):-
    (   ds_find(user, username=Auth.username, [User]),
        password_hash(Auth.password, User.salt, User.password)
    ->  Info = _{ id: User.'$id', type: User.type, key: User.key },
        debug(bc_data, 'authenticated user ~p', [Auth.username])
    ;   throw(error(user_invalid_credentials))).

%! bc_user_save(+User, -Id) is det.
%
% Saves the new user.

bc_user_save(User, Id):-
    bc_check_current_user_is_admin,
    user_save_common(User, Id).

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
    check_password_is_set(User),
    check_username_is_email(User),
    check_username_is_free(User),
    user_hash(User, Hashed),
    ds_uuid(Key),
    put_dict(key, Hashed, Key, Keyed),
    ds_insert(Keyed, Id),
    debug(bc_data, 'saved user ~p', [Id]).

%! bc_user_update(+Id, +User) is det.
%
% Updates the given user.

% FIXME user can change own data.

bc_user_update(Id, User):-
    bc_check_current_user_is_admin,
    bc_check_user_exists(Id),
    check_user_demoted_as_last_admin(Id, User),
    check_username_is_email(User),
    check_username_is_free(Id, User),
    user_hash(User, Hashed),
    put_dict('$id', Hashed, Id, Update),
    ds_update(Update),
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

%! bc_user_list(-Sorted) is det.
%
% Retrieves the list of users. Retrieved
% fields are `username`, `fullname` and `type`.

bc_user_list(Sorted):-
    bc_check_current_user_is_admin,
    ds_all(user, [username, fullname, type], Users),
    sort_dict(username, asc, Users, Sorted),
    debug(bc_data, 'retrieved the users list', []).

%! bc_user(+Id, -User) is det.
%
% Retrieves the given user. Retrieved
% fields are `username`, `fullname`, `type`, `link`, `files`.

bc_user(Id, User):-
    bc_check_current_user_is_admin,
    bc_check_user_exists(Id),
    ds_get(Id, [username, fullname, type, link, files], User),
    debug(bc_data, 'retrieved the user ~p', [User.username]).

%! bc_user_remove(+Id) is det.
%
% Removes the given user.

bc_user_remove(Id):-
    bc_check_current_user_is_admin,
    bc_check_user_exists(Id),
    check_user_has_no_posts(Id),
    check_user_is_last_admin(Id),
    ds_remove(Id),
    debug(bc_data, 'removed user ~p', [Id]).

% Produces hex-formatted hash from
% password and salt.

password_hash(Password, Salt, Hash):-
    atom_concat(Salt, Password, Data),
    sha_hash(Data, Raw, [encoding(utf8), algorithm(sha256)]),
    hash_atom(Raw, Hash).

check_username_is_free(User):-
    (   ds_find(user, username=User.username, [])
    ->  true
    ;   throw(error(user_username_exists))).

check_password_is_set(User):-
    (   get_dict(password, User, _)
    ->  true
    ;   throw(error(user_password_is_not_set))).

check_username_is_email(User):-
    atom_codes(User.username, Codes),
    (   phrase(is_email, Codes, [])
    ->  true
    ;   throw(error(user_username_is_not_email))).

is_email -->
    string_without(`@`, Start), "@", string_without(`@`, End),
    {   length(Start, LenStart), LenStart > 0,
        length(End, LenEnd), LenEnd > 0 }.

check_user_demoted_as_last_admin(UserId, Update):-
    (   Update.type = admin
    ->  true
    ;   (   is_user_last_admin(UserId)
        ->  throw(error(user_demote_last_admin))
        ;   true)).

% FIXME fetch empty list of keys.

check_username_is_free(UserId, Update):-
    (   ds_find(user, username=Update.username, [User])
    ->  (   User.'$id' = UserId
        ->  true
        ;   throw(error(user_username_exists)))
    ;   true).

% Checks whether the user is the last admin.

is_user_last_admin(UserId):-
    ds_find(user, type=admin, [type], [Admin]),
    Admin.'$id' = UserId.

check_user_has_no_posts(UserId):-
    (   ds_find(entry, author=UserId, [author], [])
    ->  true
    ;   throw(error(user_has_posts))).

check_user_is_last_admin(UserId):-
    (   is_user_last_admin(UserId)
    ->  throw(error(user_is_last_admin))
    ;   true).
