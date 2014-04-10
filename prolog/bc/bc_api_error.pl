:- module(bc_api_error, [
    bc_call_handle_error/1, % :Goal
    bc_handle_error/1       % +Error
]).

:- use_module(bc_api_io).

%! bc_call_handle_error(:Goal) is det.
%
% Calls handler through error cather
% that produces JSON reply on recognized errors.

:- meta_predicate(bc_call_handle_error(0)).

bc_call_handle_error(Goal):-
    catch(Goal, Error, true),
    (   var(Error)
    ;   bc_handle_error(Error)), !.

%! bc_handle_error(+Error) is det.
%
% Handles known errors and produces
% approriate API response.

% FIXME move into API modules?

bc_handle_error(error(invalid_api_key)):- !,
    bc_reply_error('Invalid or missing API key.').

bc_handle_error(error(invalid_input(Errors))):- !,
    format(atom(Message), 'Invalid input: ~w.', [Errors]),
    bc_reply_error(Message).

bc_handle_error(error(invalid_credentials)):- !,
    bc_reply_error('Invalid auth credentials.').

bc_handle_error(error(existing_slug(_))):- !,
    bc_reply_error('Post with the same slug exists already.').

bc_handle_error(error(unsafe_path(_))):- !,
    bc_reply_error('The file/directory path is unsafe.').

bc_handle_error(error(no_config(Name))):- !,
    format(atom(Message), 'No configuration entry ~p.', [Name]),
    bc_reply_error(Message).

bc_handle_error(error(invalid_answer(Answer))):- !,
    format(atom(Message), 'Invalid answer: ~w.', [Answer]),
    bc_reply_error(Message).

bc_handle_error(error(commenting_disabled(Id))):- !,
    format(atom(Message), 'Commenting disabled for post ~p.', [Id]),
    bc_reply_error(Message).

bc_handle_error(error(no_entry(Id))):- !,
    format(atom(Message), 'No entry ~p.', [Id]),
    bc_reply_error(Message).

bc_handle_error(error(cannot_demote_last_admin(_))):- !,
    bc_reply_error('Cannot demote the last admin.').

bc_handle_error(error(cannot_remove_last_admin(_))):- !,
    bc_reply_error('Cannot remove the last admin.').

bc_handle_error(error(user_has_existing_posts(_))):- !,
    bc_reply_error('Cannot remove user with posts.').

bc_handle_error(Error):-
    throw(Error).
