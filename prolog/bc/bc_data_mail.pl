:- module(bc_data_mail, [
    bc_mail_unsubscribe_entry/1, % +CommentId
    bc_mail_unsubscribe_all/1    % +CommentId
]).

:- use_module(library(debug)).
:- use_module(library(docstore)).

/** <module> Mail-specific data operations */

bc_mail_unsubscribe_entry(CommentId):-
    must_be(atom, CommentId),
    ds_col_get(comment, CommentId, Comment),
    debug(bc_mail, 'unsubscribe ~w from ~w comments',
        [Comment.email, Comment.post]),
    ds_find(comment,
        (post=Comment.post, email=Comment.email),
        [], Comments),
    ds_transactional(maplist(disable_notify, Comments)).

bc_mail_unsubscribe_all(CommentId):-
    must_be(atom, CommentId),
    ds_col_get(comment, CommentId, Comment),
    debug(bc_mail, 'unsubscribe ~w from all comments',
        [Comment.email]),
    ds_find(comment, email=Comment.email,
        [], Comments),
    ds_transactional(maplist(disable_notify, Comments)).

disable_notify(Comment):-
    ds_id(Comment, CommentId),
    ds_update(CommentId, _{ notify: false }).
