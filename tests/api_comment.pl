:- begin_tests(api_comment).

:- use_module(library(docstore)).

:- use_module(util).
:- use_module(util_post).
:- use_module(util_comment).

test('Add comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success").

test('Post has comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    list_comments(Post.data, Comments),
    assertion(Comments.status = "success"),
    assertion(Comments.data = [_]),
    Comments.data = [Data],
    assertion(string(Data.author)),
    assertion(integer(Data.date)).

test('Commenting not enabled', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, _{ commenting: false }, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "error").

% FIXME test with wrong answer.

:- end_tests(api_comment).
