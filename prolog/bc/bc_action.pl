:- module(bc_action, [
    bc_register_action/6,   % +Name, +Label, +Icon, +Type, +Role, :Closure
    bc_execute_access_id/3, % +Actor, +Action, +EntryId
    bc_execute/4,           % +Actor, +Action, +EntryId, -Result
    bc_available_actions/3  % +Actor, +Id, -Actions
]).

:- use_module(library(error)).

:- use_module(bc_type).
:- use_module(bc_role).
:- use_module(bc_entry).

:- dynamic(action/6).

:- meta_predicate(bc_register_action(+, +, +, +, +, 3)).

%! bc_register_action(+Name, +Label, Icon, +Type, +Role, :Closure) is det.
%
% Registers a new action.

bc_register_action(Name, Label, Icon, Type, Role, Closure):-
    must_be(atom, Name),
    must_be(atom, Label),
    must_be(atom, Icon),
    must_be(atom, Type),
    must_be(atom, Role),
    (   action(Name, _, _, _, _, _)
    ->  retractall(action(Name, _, _, _, _, _))
    ;   true),
    assertz(action(Name, Label, Icon, Type, Role, Closure)),
    debug(bc_action, 'action ~w registered', [Name]).

%! bc_execute_access_id(+Actor, +Action, +EntryId) is semidet.
%
% Checks whether the actor has execute access for the
% given action on the entry.

bc_execute_access_id(Actor, _, _):-
    Actor.type = admin, !.

bc_execute_access_id(Actor, Action, EntryId):-
    bc_entry_type(EntryId, Type),
    action(Action, _, _, Type, Actor.type, _).

% TODO: comment.

bc_execute(Actor, Action, EntryId, Result):-
    (   action(Action, _, _, _, _, Closure)
    ->  run_closure(Closure, Actor, EntryId, Result)
    ;   throw(error(no_action))).

run_closure(Closure, Actor, EntryId, Result):-
    catch_with_backtrace(
        call(Closure, Actor, EntryId, Result),
        Error,
        print_message(error, Error)),
    (   nonvar(Error)
    ->  !, throw(error(action_failed))
    ;   true).

run_closure(_, _, _, _):-
    throw(error(action_failed)).

%! bc_available_actions(+Actor, +Id, -Actions) is det.
%
% Find the list of available actions for the entry.

bc_available_actions(Actor, _, Actions):-
    Actor.type = admin, !,
    all_actions(Actions).

bc_available_actions(Actor, Id, Actions):-
    bc_entry_type(Id, Type),
    role_type_actions(Actor.type, Type, Actions).

role_type_actions(Role, Type, Actions):-
    findall(
        _{ name: Name, label: Label, icon: Icon, type: Type },
        action(Name, Label, Icon, Type, Role, _),
        Actions).

all_actions(Actions):-
    findall(
        _{ name: Name, label: Label, icon: Icon, type: Type },
        action(Name, Label, Icon, Type, _, _),
        Actions).
