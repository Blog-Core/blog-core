:- module(bc_api_type, []).

/** <module> Type API handlers */

:- use_module(library(arouter)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_type).

:- route_get(api/types,
    bc_auth, types).

:- route_get(api/type/Type,
    bc_auth, type(Type)).

types:-
    bc_actor(User),
    accessible_types(User, Types),
    bc_reply_success(Types).

type(Type):-
    bc_actor(User),
    accessible_type(User, Type, Data),
    bc_reply_success(Data).

% Entry types that user has access to.

accessible_types(User, Types):-
    findall(
        _{  name: Name,
            label: Label,
            menu_label: MenuLabel,
            grants: Grants,
            comments: Comments,
            preview: Preview },
        (
            bc_type(Name, Label, MenuLabel, Roles, Comments),
            member(Role, Roles),
            Role =.. [RoleName|Grants],
            User.type = RoleName,
            type_preview(Name, Preview)
        ), Types).

accessible_type(User, Name, Data):-
    Data = _{
        name: Name,
        label: Label,
        menu_label: MenuLabel,
        grants: Grants,
        comments: Comments,
        preview: Preview },
    bc_type(Name, Label, MenuLabel, Roles, Comments),
    member(Role, Roles),
    Role =.. [RoleName|Grants],
    User.type = RoleName,
    type_preview(Name, Preview).

accessible_type(_, _, _):-
    throw(error(no_access)).

% Gives the type preview or null
% when the preview does not exist.

type_preview(Name, Preview):-
    bc_type_preview(Name, Preview), !.

type_preview(_, null).
