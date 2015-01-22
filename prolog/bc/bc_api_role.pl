:- module(bc_api_role, []).

/** <module> Role API handlers */

:- use_module(library(arouter)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_data_role).

% Creation of new users.

:- route_get(api/roles,
    bc_auth, roles).

roles:-
    bc_actor(User),
    check_admin(User),
    findall(
        _{  name: Name,
            label: Label,
            login: Login },
        bc_role(Name, Label, Login), Roles),
    bc_reply_success(Roles).

% Checks that the user is an admin.

check_admin(User):-
    User.type = admin, !.

check_admin(_):-
    throw(error(no_access)).
