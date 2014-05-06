:- module(bc_data_cur_user, [
    bc_set_user/1,  % +User
    bc_user/1,      % -User
    bc_unset_user/0
]).

/** <module> Access to the current API user */

% Threadlocal for the current user.
% Automatically set/unset by the admin interface.

:- thread_local(user/1).

%! bc_set_user(+User) is det.
%
% Sets thread-local for the current
% user. Must be called from the REST handlers.

bc_set_user(User):-
    retractall(user(_)),
    assertz(user(User)),
    Username = User.username,
    debug(bc_data_cur_user, 'set current user to ~p', [Username]).

%! bc_user(-User) is det.
%
% Retrieves the current API user.
% Throws error(no_user_set(User))
% when no user is set.

bc_user(User):-
    (   user(User)
    ;   throw(error(no_user_set(User)))), !.

%! bc_unset_user is det.
%
% Unsets the author thread-local.
% Must be called from the REST handlers.

bc_unset_user:-
    retractall(user(_)),
    debug(bc_data_cur_user, 'user unset', []).
