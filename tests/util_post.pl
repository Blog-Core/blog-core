:- module(util_post, [
    new_post/3, % +AuthorId, +Slug, -Response
    new_post/4  % +AuthorId, +Slug, +Override, -Response
]).

:- use_module(util).

% FIXME merge into new_post/3.

new_post(AuthorId, Slug, Response):-
    new_post(AuthorId, Slug, _{}, Response).

new_post(AuthorId, Slug, Override, Response):-
    Dict = _{
        author: AuthorId,
        title: "Test post",
        slug: Slug,
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**test**",
        content_type: markdown,
        description: "Test",
        type: post
    },
    put_dict(Override, Dict, Data),
    request_post('/api/entry', Data, Response).
