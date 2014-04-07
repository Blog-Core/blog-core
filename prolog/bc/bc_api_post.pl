:- module(bc_api_post, []).

/** <module> HTTP handlers for managing posts */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_view).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_post).

% Adds new post.

:- route_post(api/post,
    bc_auth, post_save).

post_save:-
    bc_read_by_schema(post, Post),
    bc_post_save(Post, Id),
    bc_view_purge_cache,
    bc_reply_success(Id).

% Updates the given post.

:- route_put(api/post/Id,
    bc_auth, post_update(Id)).

post_update(Id):-
    bc_read_by_schema(post, Post),
    bc_post_update(Id, Post),
    bc_view_purge_cache,
    bc_reply_success(Id).

% Removes the post.

:- route_del(api/post/Id,
    bc_auth, post_remove(Id)).

post_remove(Id):-
    bc_post_remove(Id),
    bc_view_purge_cache,
    bc_reply_success(Id).

% List of all posts.
% FIXME add comment count.

:- route_get(api/posts,
    bc_auth, post_list).

post_list:-
    bc_post_list(List),
    bc_reply_success(List).

% List of posts of certain type.

:- route_get(api/posts/Type,
    bc_auth, post_list(Type)).

post_list(Type):-
    bc_post_list(Type, List),
    bc_reply_success(List).

% Single post with contents.

:- route_get(api/post/Id,
    bc_auth, post_get(Id)).

post_get(Id):-
    bc_post(Id, Post),
    bc_reply_success(Post).

% Post schema.

:- register_schema(post, _{
    type: dict,
    tag: post,
    keys: _{
        author: _{ type: atom, min_length: 36, max_length: 36 },
        title: _{ type: string, min_length: 1 },
        slug: _{ type: atom, min_length: 1 },
        tags: _{ type: list, items: atom },
        date_published: _{ type: integer, min: 0 },
        date_updated: _{ type: integer, min: 0 },
        commenting: bool,
        published: bool,
        content: string,
        content_type: _{ type: enum, values: [markdown, raw] },
        description: string,
        type: _{ type: enum, values: [page, post, block] }
    },
    optional: [ author ]
}).
