:- module(bc_data_user, [
    bc_user_save/2,   % +User, -Id
    bc_user_update/2, % +Id, +User
    bc_user_remove/1, % +Id
    bc_user_auth/2,   % +Auth, -Info
    bc_user_list/1,   % -Users
    bc_user/2         % +Id, -User
]).

:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(sha)).

%! bc_user_auth(+Auth, -Info) is semidet.
%
% Authenticates the given user. Throws
% error(invalid_credentials) when
% the auth credentials are wrong.
% Retrieves the user's API key.

bc_user_auth(Auth, Info):-
    get_dict_ex(username, Auth, Username),
    get_dict_ex(password, Auth, Password),
    debug(bc_data, 'authenticating ~p', [Username]),
    (   ds_find(user, username=Username, [User])
    ->  get_dict_ex(salt, User, Salt),
        get_dict_ex(password, User, Stored),
        atom_concat(Salt, Password, Data),
        sha_hash(Data, Hash, [encoding(utf8), algorithm(sha256)]),
        hash_atom(Hash, HashAtom),
        (   HashAtom = Stored
        ->  debug(bc_data, 'authentication successful', []),
            get_dict_ex(key, User, Key),
            get_dict_ex(type, User, Type),
            get_dict_ex('$id', User, Id),
            Info = _{ id: Id, type: Type, key: Key }
        ;   debug(bc_data, 'authentication failed', []),
            throw(error(invalid_credentials)))
    ;   throw(error(invalid_credentials))).

%! bc_user_save(+User, -Id) is det.
%
% Saves the new user. Throws
% error(existing_username(Username))
% when an user with the same username
% already exists. Attaches freshly generated
% API key to the user.

bc_user_save(User, Id):-
    get_dict_ex(username, User, Username),
    (   bc_user_username_exists(Username)
    ->  throw(error(existing_username(Username)))
    ;   bc_user_hash(User, Hashed),
        ds_uuid(Key),
        put_dict(key, Hashed, Key, Keyed),
        ds_insert(Keyed, Id),
        debug(bc_data, 'saved user ~p', [Id])).

%! bc_user_username_exists(+Username) is semidet.
%
% Checks whether an user with the given
% username exists.

bc_user_username_exists(Username):-
    ds_find(user, username=Username, Existing),
    length(Existing, Count),
    Count > 0.

%! bc_user_update(+Id, +User) is det.
%
% Updates the given user. Throws
% error(cannot_demote_last_admin(Id)) when the user
% is the last admin.
% FIXME username uniqueness

bc_user_update(Id, User):-
    get_dict_ex(type, User, Type),
    (   bc_user_last_admin(Id), Type \= admin
    ->  throw(error(cannot_demote_last_admin(Id)))
    ;   bc_user_hash(User, Hashed),
        put_dict('$id', Hashed, Id, Update),
        ds_update(Update),
        debug(bc_data, 'updated user ~p', [Id])).

% (Re)hashes the user password when password
% is set in the user dict. Replaces the password
% in user dict with salted hash. Uses freshly
% generated UUID as salt.

bc_user_hash(UserIn, UserOut):-
    (   get_dict(password, UserIn, Password)
    ->  ds_uuid(Salt),
        atom_concat(Salt, Password, Data),
        sha_hash(Data, Hash, [encoding(utf8), algorithm(sha256)]),
        hash_atom(Hash, HashAtom),
        put_dict(_{ password: HashAtom, salt: Salt }, UserIn, UserOut)
    ;   UserOut = UserIn).

% FIXME comment

bc_user_list(Sorted):-
    ds_all(user, [username, fullname, type], Users),
    sort_dict(username, asc, Users, Sorted).

% FIXME comment

bc_user(Id, User):-
    (   ds_get(Id, [username, fullname, type, link, files], User)
    ;   throw(error(no_user(Id)))), !.

%! bc_user_remove(+Id) is det.
%
% Removes the given user. Throws
% error(cannot_remove_last_admin(Id)) when
% the user is the last admin. Throws
% user_has_existing_posts(Id) when the user has
% existing posts.

bc_user_remove(Id):-
    (   bc_user_has_posts(Id)
    ->  throw(error(user_has_existing_posts(Id)))
    ;   (   bc_user_last_admin(Id)
        ->  throw(error(cannot_remove_last_admin(Id)))
        ;   ds_remove(Id),
            debug(bc_data, 'removed user ~p', [Id]))).

%! bc_user_last_admin(+Id) is semidet.
%
% Succeeds when the given user is the single admin.

bc_user_last_admin(Id):-
    ds_find(user, type=admin, [type], [Admin]),
    get_dict_ex('$id', Admin, Id).

%! bc_user_has_posts(+Id) is semidet.
%
% Checks whether the given user has posts.

bc_user_has_posts(Id):-
    bc_user_post_count(Id, Count),
    Count > 0.

%! bc_user_post_count(+Id, -Count) is det.
%
% Finds the count of posts by the given user.

bc_user_post_count(Id, Count):-
    ds_find(entry, author=Id, [author], Posts),
    length(Posts, Count).
