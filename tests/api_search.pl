:- begin_tests(api_search).

:- use_module(library(docstore)).

:- use_module(util/util).
:- use_module(util/util_user).
:- use_module(util/util_post).

test('Simple query', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    request_get('/api/search/post?q=test', Results),
    assertion(Results.status = "success"),
    assertion(Results.data = [_]).

test('With empty query', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    request_get('/api/search/post?q=', Results),
    assertion(Results.status = "success"),
    assertion(Results.data = []).

test('With no results', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    request_get('/api/search/post?q=notexiststoken', Results),
    assertion(Results.status = "success"),
    assertion(Results.data = []).

:- end_tests(api_search).
