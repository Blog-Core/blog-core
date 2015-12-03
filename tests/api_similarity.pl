:- begin_tests(api_similarity).

:- use_module(library(docstore)).

:- use_module(util/util).
:- use_module(util/util_post).
:- use_module(util/util_user).
:- use_module(util/util_comment).

test('No similar posts', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    atom_concat('/api/similar/post/public/', Post.data, URL),
    request_get(URL, Response),
    assertion(Response.status = "success"),
    assertion(Response.data = []).

test('One similar post', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post1, Post1),
    assertion(Post1.status = "success"),
    new_post(AuthorId, test_post2, Post2),
    assertion(Post2.status = "success"),
    atom_concat('/api/similar/post/public/', Post1.data, URL),
    request_get(URL, Response),
    assertion(Response.status = "success"),
    assertion(Response.data = [_]),
    Response.data = [Entry],
    Entry.entry.'$id' = Post2.data.

:- end_tests(api_similarity).
