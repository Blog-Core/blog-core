:- begin_tests(api_tag).

:- use_module(library(docstore)).

:- use_module(util/util).
:- use_module(util/util_post).
:- use_module(util/util_user).
:- use_module(util/util_comment).

test('New entry tags, public', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    request_get('/api/tags/post/public', Response),
    assertion(Response.status = "success"),
    assertion(Response.data = [_, _]),
    Response.data = Tags,
    member(Tag, Tags),
    Tag.tag = "test", !.

test('New entry tags, all', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    request_get('/api/tags/post/all', Response),
    assertion(Response.status = "success"),
    assertion(Response.data = [_, _]),
    Response.data = Tags,
    member(Tag, Tags),
    Tag.tag = "test", !.

:- end_tests(api_tag).
