:- module(bc_api_actor, [
    bc_set_actor/1,  % +User
    bc_actor/1,      % -User
    bc_unset_actor/0
]).

/** <module> Access to the current API user */

% Threadlocal for the current user.
% Automatically set/unset by the admin interface.

:- thread_local(actor/1).

%! bc_set_actor(+User) is det.
%
% Sets thread-local for the current
% user. Must be called from the REST handlers.

bc_set_actor(User):-
    retractall(actor(_)),
    assertz(actor(User)),
    Username = User.username,
    debug(bc_api_actor, 'set current actor to ~p', [Username]).

%! bc_actor(-User) is det.
%
% Retrieves the current API user.
% Throws error(no_actor_set)
% when no user is set.

bc_actor(User):-
    (   actor(User)
    ;   throw(error(no_actor_set))), !.

%! bc_unset_actor is det.
%
% Unsets the author thread-local.
% Must be called from the REST handlers.

bc_unset_actor:-
    retractall(actor(_)),
    debug(bc_data_cur_actor, 'actor unset', []).
