:- module(bc_api_type, []).

/** <module> Type API handlers */

:- use_module(library(arouter)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_type).
:- use_module(bc_data_cur_user).

% Creation of new users.

:- route_get(api/types,
    bc_auth, types).

types:-
    bc_user(User),
    accessible_types(User, Types),
    bc_reply_success(Types).

% Entry types that user has access to.

accessible_types(User, Types):-
    findall(
        _{  name: Name,
            label: Label,
            menu_label: MenuLabel,
            roles: Roles },
        (
            bc_type(Name, Label, MenuLabel, Roles),
            member(User.type, Roles)
        ), Types).
