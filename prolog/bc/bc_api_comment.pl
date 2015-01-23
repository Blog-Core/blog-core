:- module(bc_api_comment, []).

:- use_module(library(dict_schema)).
:- use_module(library(arouter)).

:- use_module(bc_view).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_error).
:- use_module(bc_api_actor).
:- use_module(bc_data_comment).
:- use_module(bc_comment_question).

% Comments of a single post.
% For admin interface.

:- route_get(api/post/Id/comments,
    bc_auth, comment_tree(Id)).

comment_tree(PostId):-
    bc_comment_tree(PostId, Comments),
    bc_reply_success(Comments).

% Adds new comment. This is available for
% everyone.

:- route_post(api/post/Id/comment,
    bc_call_handle_error, comment_save(Id)).

comment_save(PostId):-
    bc_read_by_schema(comment, Comment),
    bc_comment_save(PostId, Comment, CommentId),
    bc_view_purge_cache,
    bc_reply_success(CommentId).

% Removes the given comment.

:- route_del(api/comment/EntryId/Id,
    bc_auth, comment_remove(EntryId, Id)).

comment_remove(EntryId, Id):-
    bc_actor(Actor),
    bc_comment_remove(Actor, EntryId, Id),
    bc_view_purge_cache,
    bc_reply_success(Id).

% Human test question. Used in
% comment form.

:- route_get(api/question, comment_question).

comment_question:-
    bc_random_question(Id, Question),
    bc_reply_success(_{ id: Id, question: Question }).

% Basic comments. Can be overriden to
% add more properties.

:- register_schema(comment, _{
    type: dict,
    tag: comment,
    keys: _{
        author: _{ type: string, min_length: 1 },
        content: _{ type: string, min_length: 1 },
        reply_to: _{ type: atom, min_length: 1 },
        email: _{ type: string, min_length: 1 },
        site: _{ type: string, min_length: 1 },
        question: integer,
        answer: atom
    },
    optional: [ reply_to, email, site ]
}).
