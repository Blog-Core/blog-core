:- module(bc_data_type, [
    bc_register_type/5,
    bc_unregister_type/1,
    bc_type/5
]).

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(bc_data_role).

:- dynamic(type/5).

%! bc_type(Name, Label, MenuLabel, Roles, Comments) is nondet.
%
% Matches/generates all registered types.

bc_type(Name, Label, MenuLabel, Roles, Comments):-
    type(Name, Label, MenuLabel, Roles, Comments).

%! bc_register_type(+Name, +Label, +MenuLabel, +Roles, +Comments) is det.
%
% Registers a new type. Overwrites existing type.

bc_register_type(Name, Label, MenuLabel, Roles, Comments):-
    must_be(atom, Name),
    must_be(atom, Label),
    must_be(atom, MenuLabel),
    check_roles(Roles),
    check_roles_duplicate(Roles),
    (   type(Name, _, _, _, _)
    ->  retractall(type(Name, _, _, _, _))
    ;   true),
    assertz(type(Name, Label, MenuLabel, Roles, Comments)),
    debug(bc_data_type, 'type ~w registered', [Name]).

%! bc_unregister_type(+Name) is det.
%
% Removes the given type. Does nothing
% when the type does not exist already.

bc_unregister_type(Name):-
    must_be(atom, Name),
    retractall(type(Name, _, _, _, _)).

% Checks that type gets valid
% access roles.

check_roles([]).

check_roles([Role|Roles]):-
    check_role(Role),
    check_roles(Roles).

% Checks that there are no
% duplicate roles.

check_roles_duplicate(Roles):-
    maplist(role_name, Roles, Names),
    sort(Names, Sorted),
    length(Names, Len),
    length(Sorted, Len).

check_roles_duplicate(_):-
    throw(error(duplicate_roles)).

role_name(Role, Name):-
    Role =.. [Name|_].

check_role(Role):-
    Role =.. [Name|Grants],
    check_role_exists(Name),
    check_grants(Grants).

check_role_exists(Name):-
    bc_role(Name, _, _, _), !.

check_role_exists(_):-
    throw(error(role_not_exists)).

check_grants([Grant|Grants]):-
    check_grant(Grant),
    check_grants(Grants).

check_grants([]).

check_grant(Grant):-
    nonvar(Grant),
    memberchk(Grant, [create, read_own, update_own, remove_own, read_all, update_all, remove_all]), !.

check_grant(_):-
    throw(error(invalid_grant)).
