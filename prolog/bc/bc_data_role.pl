:- module(bc_data_role, [
    bc_register_role/4,
    bc_unregister_role/1,
    bc_role/4
]).

:- use_module(library(debug)).
:- use_module(library(error)).

:- dynamic(role/4).

%! bc_role(Name, Label, Login, Files) is nondet.
%
% Matches/generates all registered roles.

bc_role(Name, Label, Login, Files):-
    role(Name, Label, Login, Files).

%! bc_register_role(+Name, +Label, +Login, +Files) is det.
%
% Registers a new role. Overwrites existing role.

bc_register_role(Name, Label, Login, Files):-
    must_be(atom, Name),
    must_be(atom, Label),
    must_be(boolean, Login),
    must_be(boolean, Files),
    (   role(Name, _, _, _)
    ->  retractall(role(Name, _, _, _))
    ;   true),
    assertz(role(Name, Label, Login, Files)),
    debug(bc_data_role, 'type ~w registered', [Name]).

%! bc_unregister_role(+Name) is det.
%
% Removes the given role. Does nothing
% when the role does not exist already.

bc_unregister_role(Name):-
    must_be(atom, Name),
    retractall(role(Name, _, _, _)).
