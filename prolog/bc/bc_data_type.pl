:- module(bc_data_type, [
    bc_register_type/4,
    bc_unregister_type/1,
    bc_type/4
]).

:- use_module(library(debug)).
:- use_module(library(error)).

%! bc_type(Name, Label, MenuLabel, Roles) is nondet.
%
% Matches/generates all registered types.

bc_type(Name, Label, MenuLabel, Roles):-
    type(Name, Label, MenuLabel, Roles).

:- dynamic(type/4).

%! bc_register_type(+Name, +Label, +MenuLabel, +Roles) is det.
%
% Registers a new type. Does nothing when a type
% with a same name already exists.

bc_register_type(Name, Label, MenuLabel, Roles):-
    must_be(atom, Name),
    must_be(atom, Label),
    must_be(atom, MenuLabel),
    must_be(list(atom), Roles),
    check_roles(Roles),
    (   type(Name, _, _, _)
    ->  true
    ;   assertz(type(Name, Label, MenuLabel, Roles)),
        debug(bc_data_types, 'type ~w registered', [Name])).

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
    (   check_role(Role)
    ->  check_roles(Roles)
    ;   throw(error(invalid_role(Role)))).

check_role(author).
check_role(admin).
