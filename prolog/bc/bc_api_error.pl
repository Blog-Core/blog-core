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

bc_handle_error(error(user_invalid_credentials)):- !,
    bc_reply_error('Invalid auth credentials.').

bc_handle_error(error(user_role_no_login)):- !,
    bc_reply_error('Assigned role does not permit login.').

bc_handle_error(error(user_password_is_not_set)):- !,
    bc_reply_error('The user password is not set.').

bc_handle_error(error(user_username_is_not_email)):- !,
    bc_reply_error('The username is not an email address.').

bc_handle_error(error(user_username_exists)):- !,
    bc_reply_error('The username exists.').

bc_handle_error(error(user_invalid_role)):- !,
    bc_reply_error('The user role is not valid.').

bc_handle_error(error(user_current_is_not_admin)):- !,
    bc_reply_error('The operation requires admin privileges.').

bc_handle_error(error(user_demote_last_admin)):- !,
    bc_reply_error('Cannot demote the last admin.').

bc_handle_error(error(user_not_exists)):- !,
    bc_reply_error('The user does not exist.').

bc_handle_error(error(user_has_posts)):- !,
    bc_reply_error('The user has posts.').

bc_handle_error(error(user_is_last_admin)):- !,
    bc_reply_error('Cannot remove the last admin.').

bc_handle_error(error(existing_slug)):- !,
    bc_reply_error('The entry with the same slug exists already.').

bc_handle_error(error(entry_not_exists)):- !,
    bc_reply_error('The entry does not exist.').

bc_handle_error(error(no_ownership)):- !,
    bc_reply_error('The operation requires ownership privileges.').

bc_handle_error(error(no_type_access)):- !,
    bc_reply_error('The operation requires entry type access.').

bc_handle_error(error(no_access)):- !,
    bc_reply_error('The operation requires access privileges.').

bc_handle_error(error(unsafe_path(_))):- !,
    bc_reply_error('The file/directory path is unsafe.').

bc_handle_error(error(directory_exists)):- !,
    bc_reply_error('The directory exists.').

bc_handle_error(error(file_exists)):- !,
    bc_reply_error('The file exists.').

bc_handle_error(error(no_files_access)):- !,
    bc_reply_error('No permission to manage files.').

bc_handle_error(error(comment_invalid_answer)):- !,
    bc_reply_error('The human question answer is wrong.').

bc_handle_error(error(entry_commenting_disabled)):- !,
    bc_reply_error('Commenting is disabled for the entry.').

bc_handle_error(error(comment_reply_wrong_post)):- !,
    bc_reply_error('The replied-to comment and the entry do not match.').

bc_handle_error(error(user_has_existing_posts(_))):- !,
    bc_reply_error('Cannot remove user with posts.').

bc_handle_error(error(comment_not_exists)):- !,
    bc_reply_error('The comment replied to does not exist.').

bc_handle_error(error(reply_post_id_mismatch(_))):- !,
    bc_reply_error('The comment replied to does not match the post.').

bc_handle_error(Error):-
    throw(Error).
