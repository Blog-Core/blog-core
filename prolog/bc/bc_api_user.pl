:- module(bc_api_user, []).

/** <module> User management API handlers */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_user).
:- use_module(bc_data_cur_user).

% Creation of new users.

:- route_post(api/user,
    bc_auth, user_save).

user_save:-
    bc_read_by_schema(user, User),
    bc_user_save(User, Id),
    bc_reply_success(Id).

% Updates given user.

:- route_put(api/user/Id,
    bc_auth, user_update(Id)).

user_update(Id):-
    bc_read_by_schema(user, User),
    bc_user_update(Id, User),
    bc_reply_success(Id).

% Removes an user.

:- route_del(api/user/Id,
    bc_auth, user_remove(Id)).

user_remove(Id):-
    bc_user_remove(Id),
    bc_reply_success(Id).

% Sends list of users.

:- route_get(api/users,
    bc_auth, users_list).

users_list:-
    bc_user_list(Users),
    bc_reply_success(Users).

% Sends the given user.

:- route_get(api/user/Id,
    bc_auth, user_get(Id)).

user_get(Id):-
    bc_user(Id, User),
    bc_reply_success(User).

% Sends the given user information.

:- route_get(api/user/info,
    bc_auth, user_info).

user_info:-
    bc_user(User),
    bc_reply_success(_{
        '$id': User.'$id',
        type: User.type,
        username: User.username }).

% Schema for user data.

:- register_schema(user, _{
    type: dict,
    tag: user,
    keys: _{
        fullname: _{ type: atom, min_length: 1 },
        username: _{ type: atom, min_length: 1 },
        password: _{ type: atom, min_length: 6 },
        type: _{ type: atom, min_length: 1, max_length: 100 },
        link: string,
        files: bool
    },
    optional: [ password ]
}).
