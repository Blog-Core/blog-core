:- module(bc_api_type, []).

/** <module> Type API handlers */

:- use_module(library(arouter)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_type).

:- route_get(api/types,
    bc_auth, types).

types:-
    bc_actor(User),
    accessible_types(User, Types),
    bc_reply_success(Types).

% Entry types that user has access to.

accessible_types(User, Types):-
    findall(
        _{  name: Name,
            label: Label,
            menu_label: MenuLabel,
            grants: Grants,
            comments: Comments },
        (
            bc_type(Name, Label, MenuLabel, Roles, Comments),
            member(Role, Roles),
            Role =.. [RoleName|Grants],
            User.type = RoleName
        ), Types).
