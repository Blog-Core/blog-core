:- module(bc_data, [
    bc_data_open/1,         % +File
    bc_data_close/0,
    bc_post_save/2,         % +Post, -Id
    bc_post_update/2,       % +Id, +Post
    bc_post_remove/1,       % +Id
    bc_post_find_by_slug/2, % +Slug, -Post
    bc_user_save/2,         % +User, -Id
    bc_user_update/2,       % +Id, +User
    bc_user_remove/1,       % +Id
    bc_user_auth/2,         % +Auth, -Key
    bc_set_user/1,          % +User
    bc_unset_user/0,
    bc_config_get/2,        % +Name, -Value
    bc_config_set/2         % +Name, +Value
]).

:- use_module(library(docstore)).
:- use_module(library(debug)).
:- use_module(library(sha)).
:- use_module(library(md/md_parse)).

% Threadlocal for the current user.
% Automatically set/unset by the admin interface.

:- thread_local(user/1).

%! bc_set_user(+User) is det.
%
% Sets thread-local for the current
% user. Must be called from the REST handlers.

bc_set_user(User):-
    retractall(user(_)),
    assertz(user(User)),
    get_dict_ex(username, User, Username),
    debug(bc_data, 'set current user to ~p', [Username]).

%! bc_unset_user is det.
%
% Unsets the author thread-local.
% Must be called from the REST handlers.

bc_unset_user:-
    retractall(user(_)),
    debug(bc_data, 'user unset', []).

%! bc_data_open(+File) is det.
%
% Opens the docstore database file.
% Inserts the initial data.

bc_data_open(File):-
    ds_open(File),
    bc_init,
    debug(bc_data, 'opened docstore file ~p', [File]).

%! bc_data_close is det.
%
% Closes the docstore database.

bc_data_close:-
    ds_close,
    debug(bc_data, 'closed docstore file', []).

bc_post_save(Post, Id):-
    get_dict_ex(slug, Post, Slug),
    ds_find(post, slug=Slug, Existing),
    length(Existing, Length),
    (   Length > 0
    ->  throw(error(existing_slug(Slug)))
    ;   bc_post_format(Post, Formatted),
        ds_insert(Formatted, Id),
        debug(bc_data, 'saved post ~p', [Id])).

%! bc_post_update(+Id, +Post) is det.
%
% Updates the given post. Reformats HTML.

bc_post_update(Id, Post):-
    bc_post_format(Post, Formatted),
    put_dict('$id', Formatted, Id, Update),
    ds_update(Update),
    debug(bc_data, 'updated post ~p', [Id]).

% Formats post HTML contents based on
% the post's content type.

bc_post_format(PostIn, PostOut):-
    get_dict_ex(content, PostIn, Content),
    get_dict_ex(content_type, PostIn, ContentType),
    (   ContentType = markdown
    ->  md_html_string(Content, Html)
    ;   Html = Content),
    put_dict(_{ html: Html }, PostIn, PostOut).

%! bc_post_remove(+Id) is det.
%
% Removes the given post and its comments.

bc_post_remove(Id):-
    ds_remove(Id),
    ds_remove(comment, post=Id),
    debug(bc_data, 'removed post ~p', [Id]).

%! bc_post_find_by_slug(+Slug, -Post) is semidet.
%
% Finds post. Fails when no post is found.

bc_post_find_by_slug(Slug, Post):-
    ds_find(post, slug=Slug, [Post]).

%! bc_user_auth(+Auth, -Key) is semidet.
%
% Authenticates the given user. Throws
% error(invalid_credentials) when
% the auth credentials are wrong.
% Retrieves the user's API key.

bc_user_auth(Auth, Key):-
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
            get_dict_ex(key, User, Key)
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
    ds_find(user, username = Username, Existing),
    length(Existing, Count),
    Count > 0.

%! bc_user_update(+Id, +User) is det.
%
% Updates the given user. Throws
% error(cannot_demote_last_admin(Id)) when the user
% is the last admin.

bc_user_update(Id, User):-
    get_dict_ex(type, User, Type),
    (   bc_user_last_admin(Id), Type=normal
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
    ds_uuid(Salt),
    get_dict_ex(password, UserIn, Password),
    atom_concat(Salt, Password, Data),
    sha_hash(Data, Hash, [encoding(utf8), algorithm(sha256)]),
    hash_atom(Hash, HashAtom),
    put_dict(_{ password: HashAtom, salt: Salt }, UserIn, UserOut).

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
    ds_find(post, author=Id, [author], Posts),
    length(Posts, Count).

%! bc_config_get(+Name, -Value) is det.
%
% Retrieves the configuration entry.
% When the entry does not exist then
% an error error(no_config(Name)) is thrown.

bc_config_get(Name, Value):-
    (   ds_find(config, name=Name, [Doc])
    ->  get_dict_ex(value, Doc, Value)
    ;   throw(error(no_config(Name)))).

%! bc_config_set(+Name, +Value) is det.
%
% Sets the configuration value. If the
% value does not exist yet, it is added.

bc_config_set(Name, Value):-
    debug(bc_data, 'setting ~w to ~p', [Name, Value]),
    (   ds_find(config, name=Name, [Doc])
    ->  put_dict(value, Doc, Value, New),
        ds_update(New)
    ;   ds_insert(config{ name: Name, value: Value })).

% Sets up initial values.
% Inserts the default admin user.

bc_init:-
    ds_all(config, []), !,
    debug(bc_data, 'inserting initial data', []),
    bc_config_set(title, 'Untitled site'),
    bc_user_save(user{
        fullname: 'Admin',
        username: 'admin',
        password: 'admin',
        type: admin
    }, _).

bc_init.
