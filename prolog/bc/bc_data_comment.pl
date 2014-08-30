:- module(bc_data_comment, [
    bc_comment_tree/2,      % +EntryId, -Comments
    bc_comment_save/3,      % +EntryId, +Comment, -CommentId
    bc_comment_remove/1     % +EntryId
]).

/** <module> Handles post comments */

:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(debug)).

:- use_module(bc_data_validate).
:- use_module(bc_data_comment_question).
:- use_module(bc_data_comment_format).
:- use_module(bc_data_comment_tree).
:- use_module(bc_data_cur_user).

%! bc_comment_tree(+EntryId, -Tree) is det.
%
% Retrieves the tree of comments for the post.

bc_comment_tree(EntryId, Tree):-
    bc_check_entry_exists(EntryId),
    ds_find(comment, post = EntryId, Comments),
    sort_dict(date, desc, Comments, Sorted),
    bc_build_comment_tree(Sorted, Tree),
    debug(bc_data, 'retrieved comment tree for ~p', [ EntryId ]).

%! bc_comment_save(+EntryId, +Comment, -CommentId) is det.
%
% Saves a new comment.

bc_comment_save(EntryId, Comment, CommentId):-
    bc_check_entry_exists(EntryId),
    check_commenting_enabled(EntryId),
    check_answer(Comment),
    check_reply_to_exists(Comment),
    check_reply_to_same_entry(EntryId, Comment),
    comment_save(EntryId, Comment, CommentId),
    debug(bc_data, 'saved comment ~p', [ CommentId ]).

% Attaches comment timestamp,
% formats comment content and
% saves into docstore.

comment_save(EntryId, Comment, CommentId):-
    bc_format_comment(Comment.content, Formatted),
    get_time(Time),
    Ts is floor(Time),
    put_dict(_{
        date: Ts,
        post: EntryId,
        html: Formatted }, Comment, Processed),
    ds_insert(Processed, CommentId).

%! bc_comment_remove(+CommentId) is det.
%
% Removes the given comment.

bc_comment_remove(CommentId):-
    bc_check_comment_exists(CommentId),
    check_is_own_comment(CommentId),
    comment_remove_rec(CommentId),
    debug(bc_data, 'removed comment ~p', [ CommentId ]).

% FIXME fetch empty list of keys

comment_remove_rec(CommentId):-
    ds_find(comment, reply_to = CommentId, [ author ], Replies),
    comment_remove_rec_list(Replies),
    ds_remove(CommentId).

comment_remove_rec_list([Comment|Comments]):-
    comment_remove_rec(Comment.'$id'),
    comment_remove_rec_list(Comments).

comment_remove_rec_list([]).

% Checks that comment can be removed
% by the current user.

check_is_own_comment(CommentId):-
    bc_user(User),
    (   User.type = admin
    ->  true
    ;   ds_get(CommentId, [ post ], Comment),
        ds_get(Comment.post, [ author ], Post),
        (   Post.author = User.'$id'
        ->  true
        ;   throw(error(user_current_is_not_admin)))).

% Checks that the comment replied-to
% actually exists.

check_reply_to_exists(Comment):-
    (   get_dict(reply_to, Comment, ReplyTo)
    ->  bc_check_comment_exists(ReplyTo)
    ;   true).

% Checks that the comment is reply
% for another comment under the given post.

check_reply_to_same_entry(EntryId, Comment):-
    (   get_dict(reply_to, Comment, ReplyTo)
    ->  (   ds_get(ReplyTo, [ post ], RepliedTo)
        ->  (   RepliedTo.post = EntryId
            ->  true
            ;   throw(error(comment_reply_wrong_post)))
        ;   true)
    ;   true).

% Checks that the commenting is
% enabled for the entry.

check_commenting_enabled(EntryId):-
    (   ds_get(EntryId, [ commenting ], Entry)
    ->  (   Entry.commenting = true
        ->  true
        ;   throw(error(entry_commenting_disabled)))
    ;   true).

% Checks the human validation
% question answer.

check_answer(Comment):-
    (   bc_answer_ok(Comment.question, Comment.answer)
    ->  true
    ;   throw(error(comment_invalid_answer))).
