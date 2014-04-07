:- module(bc_api_user, []).

/** <module> User management API handlers */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_user).

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

% Schema for user data.

:- register_schema(user, _{
    type: dict,
    tag: user,
    keys: _{
        fullname: _{ type: atom, min_length: 1 },
        username: _{ type: atom, min_length: 1 },
        password: _{ type: atom, min_length: 6 },
        type: _{ type: enum, values: [author, admin] }
    }
}).
