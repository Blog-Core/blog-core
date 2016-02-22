:- module(bc_data_comment, [
    bc_comment_tree/2,      % +EntryId, -Comments
    bc_comment_save/3,      % +EntryId, +Comment, -Id
    bc_comment_remove/3     % +Actor, +EntryId, +Id
]).

/** <module> Handles post comments */

:- use_module(library(sort_dict)).
:- use_module(library(docstore)).
:- use_module(library(debug)).

:- use_module(bc_mail).
:- use_module(bc_entry).
:- use_module(bc_comment).
:- use_module(bc_access).
:- use_module(bc_type).
:- use_module(bc_data_config).
:- use_module(bc_comment_question).
:- use_module(bc_comment_format).
:- use_module(bc_comment_tree).
:- use_module(bc_comment_notify).

%! bc_comment_tree(+EntryId, -Tree) is det.
%
% Retrieves the tree of comments for the post.

bc_comment_tree(EntryId, Tree):-
    bc_entry_exists(EntryId),
    ds_find(comment, post=EntryId, Comments),
    sort_dict(date, desc, Comments, Sorted),
    bc_build_comment_tree(Sorted, Tree),
    debug(bc_data, 'retrieved comment tree for ~p', [EntryId]).

%! bc_comment_save(+EntryId, +Comment, -Id) is det.
%
% Saves a new comment.

bc_comment_save(EntryId, Comment, Id):-
    can_create(EntryId, Comment),
    comment_save(EntryId, Comment, Id),
    bc_comment_notify(Id),
    debug(bc_data, 'saved comment ~p', [Id]).

can_create(EntryId, Comment):-
    bc_entry_exists(EntryId),
    comments_allowed(EntryId),
    (   get_dict(reply_to, Comment, To)
    ->  bc_comment_exists(EntryId, To)
    ;   true),
    correct_answer(Comment).

comments_allowed(EntryId):-
    bc_entry_commenting(EntryId, true),
    bc_entry_type(EntryId, Type),
    bc_type(Type, _, _, _, true), !.

comments_allowed(_):-
    throw(error(no_comments_allowed)).

correct_answer(Comment):-
    bc_answer_ok(Comment.question, Comment.answer), !.

correct_answer(_):-
    throw(error(incorrect_answer)).

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

%! bc_comment_remove(+Actor, +EntryId, +Id) is det.
%
% Removes the given comment.
% Requires update access on the entry.

bc_comment_remove(Actor, EntryId, Id):-
    can_remove(Actor, EntryId, Id),
    bc_comment_remove(Id),
    debug(bc_data, 'removed comment ~p', [Id]).

can_remove(Actor, EntryId, Id):-
    bc_comment_exists(EntryId, Id),
    remove_access(Actor, EntryId).

remove_access(Actor, EntryId):-
    bc_update_access_id(Actor, EntryId), !.

remove_access(_, _):-
    throw(error(no_access)).
