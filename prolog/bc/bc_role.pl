:- module(bc_role, [
    bc_register_role/3,
    bc_unregister_role/1,
    bc_role/3
]).

:- use_module(library(debug)).
:- use_module(library(error)).

:- dynamic(role/3).

%! bc_role(Name, Label, Login) is nondet.
%
% Matches/generates all registered roles.

bc_role(Name, Label, Login):-
    role(Name, Label, Login).

%! bc_register_role(+Name, +Label, +Login) is det.
%
% Registers a new role. Overwrites existing role.

bc_register_role(Name, Label, Login):-
    must_be(atom, Name),
    must_be(atom, Label),
    must_be(boolean, Login),
    (   role(Name, _, _)
    ->  retractall(role(Name, _, _))
    ;   true),
    assertz(role(Name, Label, Login)),
    debug(bc_role, 'type ~w registered', [Name]).

%! bc_unregister_role(+Name) is det.
%
% Removes the given role. Does nothing
% when the role does not exist already.

bc_unregister_role(Name):-
    must_be(atom, Name),
    retractall(role(Name, _, _)).
