:- module(bc_data_validate, [
    bc_check_entry_exists/1,   % +EntryId
    bc_check_comment_exists/1, % +CommentId
    bc_check_user_exists/1,    % +UserId
    bc_check_current_user_is_admin/0
]).

/** <module> Data validation helpers */

:- use_module(library(docstore)).
:- use_module(bc_data_cur_user).

%! bc_check_entry_exists(+EntryId) is det.
%
% Checks that the given entry exists. Throws
% error when it does not.

bc_check_entry_exists(EntryId):-
    (   exists_document(EntryId, entry)
    ->  true
    ;   throw(error(entry_not_exists))).

%! bc_check_comment_exists(+EntryId) is det.
%
% Checks that the given comment exists. Throws
% error when it does not.

bc_check_comment_exists(CommentId):-
    (   exists_document(CommentId, comment)
    ->  true
    ;   throw(error(comment_not_exists))).

%! bc_check_user_exists(+UserId) is det.
%
% Checks that the given user exists. Throws
% error when it does not.

bc_check_user_exists(UserId):-
    (   exists_document(UserId, user)
    ->  true
    ;   throw(error(user_not_exists))).

% FIXME use better method.

exists_document(Id, Col):-
    ds_get(Id, Doc),
    is_dict(Doc, Col).

% FIXME refactor out current user id.

bc_check_current_user_is_admin:-
    (   bc_user(User), User.type = admin
    ->  true
    ;   throw(error(user_current_is_not_admin))).
