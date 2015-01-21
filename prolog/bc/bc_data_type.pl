:- module(bc_data_type, [
    bc_register_type/4,
    bc_unregister_type/1,
    bc_type/4
]).

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(bc_data_role).

:- dynamic(type/4).

%! bc_type(Name, Label, MenuLabel, Roles) is nondet.
%
% Matches/generates all registered types.

bc_type(Name, Label, MenuLabel, Roles):-
    type(Name, Label, MenuLabel, Roles).

%! bc_register_type(+Name, +Label, +MenuLabel, +Roles) is det.
%
% Registers a new type. Overwrites existing type.

bc_register_type(Name, Label, MenuLabel, Roles):-
    must_be(atom, Name),
    must_be(atom, Label),
    must_be(atom, MenuLabel),
    must_be(list(atom), Roles),
    check_roles(Roles),
    (   type(Name, _, _, _)
    ->  retractall(type(Name, _, _, _))
    ;   true),
    assertz(type(Name, Label, MenuLabel, Roles)),
    debug(bc_data_type, 'type ~w registered', [Name]).

%! bc_unregister_type(+Name) is det.
%
% Removes the given type. Does nothing
% when the type does not exist already.

bc_unregister_type(Name):-
    must_be(atom, Name),
    retractall(type(Name, _, _, _)).

% Checks that type gets valid
% access roles.

check_roles([]).

check_roles([Role|Roles]):-
    check_role(Role),
    check_roles(Roles).

check_role(Role):-
    (   bc_role(Role, _, Login, _)
    ->  (   Login = false
        ->  throw(error(role_cannot_login(Role)))
        ;   true)
    ;   throw(error(role_not_exists(Role)))).
