:- begin_tests(api_comment_reply_to).

:- use_module(library(docstore)).

:- use_module(util).
:- use_module(util_post).
:- use_module(util_comment).

test('Reply to comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_comment(Post.data, Comment.data, Reply),
    assertion(Reply.status = "success").

test('Reply to non-existent comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, 'xxx-comment-not-exists', Reply),
    assertion(Reply.status = "error").

test('Reply to non-existent comment and post', [setup(new_database)]):-
    new_comment('xxx-post-not-exists', 'xxx-not-exists', Reply),
    assertion(Reply.status = "error").

test('Reply to comment under other post', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_post(AuthorId, test_other_post, OtherPost),
    assertion(OtherPost.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_comment(OtherPost.data, Comment.data, Reply),
    assertion(Reply.status = "error").

test('Reply to comment, retrieve the tree', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_comment(Post.data, Comment.data, Reply),
    assertion(Reply.status = "success"),
    new_comment(Post.data, OtherComment),
    assertion(OtherComment.status = "success"),
    list_comments(Post.data, Comments),
    assertion(Comments.status = "success"),
    assertion(Comments.data = [_, _]),
    Comments.data = [Second, First],
    assertion(First.replies = [_]),
    assertion(Second.replies = []).

:- end_tests(api_comment_reply_to).
