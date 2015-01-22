:- module(bc_comment, [
    bc_comment_exists/2, % +EntryId, +Id
    bc_comment_remove/1  % +Id
]).

:- use_module(library(docstore)).

% Checks that the comment exists.

bc_comment_exists(EntryId, Id):-
    ds_get(Id, [post], Comment),
    Comment.post = EntryId, !.

bc_comment_exists(_, _):-
    throw(error(comment_not_exists)).

% Removes comment tree under
% the given comment.

bc_comment_remove(Id):-
    ds_find(comment, reply_to=Id, [author], Replies),
    comment_remove_list(Replies),
    ds_remove(Id).

comment_remove_list([Comment|Comments]):-
    bc_comment_remove(Comment.'$id'),
    comment_remove_list(Comments).

comment_remove_list([]).
