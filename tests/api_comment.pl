:- begin_tests(api_comment).

:- use_module(library(http/http_open)).
:- use_module(library(docstore)).

:- use_module(util/util).
:- use_module(util/util_user).
:- use_module(util/util_post).
:- use_module(util/util_comment).

:- use_module(prolog/bc/bc_mail_queue).

test('Add comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success").

test('Add comment, wrong answer', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, _{ answer: wrong }, Comment),
    assertion(Comment.status = "error"),
    assertion(Comment.message = "The human question answer is wrong.").

test('Add comment, invalid data', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, _{ content: "" }, Comment),
    assertion(is_invalid_data(Comment)).

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
    assertion(Comment.status = "error"),
    assertion(Comment.message = "No comments are allowed on the entry.").

test('Reply to comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_comment(Post.data, _{ reply_to: Comment.data }, Reply),
    assertion(Reply.status = "success").

test('Reply to non-existent comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, _{ reply_to: 'xxx-comment-not-exists' }, Reply),
    assertion(Reply.status = "error"),
    assertion(Reply.message = "The comment replied to does not exist.").

test('Reply to non-existent comment and post', [setup(new_database)]):-
    new_comment('xxx-post-not-exists', _{ reply_to: 'xxx-not-exists' }, Reply),
    assertion(Reply.status = "error"),
    assertion(Reply.message = "The entry does not exist.").

test('Reply to comment under other post', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_post(AuthorId, test_other_post, OtherPost),
    assertion(OtherPost.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_comment(OtherPost.data, _{ reply_to: Comment.data }, Reply),
    assertion(Reply.status = "error"),
    assertion(Reply.message = "The comment replied to does not exist.").

test('Reply to comment, retrieve the tree', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_comment(Post.data, _{ reply_to: Comment.data }, Reply),
    assertion(Reply.status = "success"),
    new_comment(Post.data, OtherComment),
    assertion(OtherComment.status = "success"),
    list_comments(Post.data, Comments),
    assertion(Comments.status = "success"),
    assertion(Comments.data = [_, _]),
    Comments.data = [Second, First],
    assertion(First.replies = [_]),
    assertion(Second.replies = []).

test('Retrieve the post comments tree, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    list_comments(Post.data, Comments),
    assertion(Comments.status = "error"),
    assertion(Comments.message = "Invalid or missing API key.").

test('Retrieve the non-existent post comments tree', [setup(new_database)]):-
    list_comments('xxx-non-existent-entry', Comments),
    assertion(Comments.status = "error"),
    assertion(Comments.message = "The entry does not exist.").

test('Remove comment', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    remove_comment(Post.data, Comment.data, Removal),
    assertion(Removal.status = "success").

test('Remove comment, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    set_no_auth,
    remove_comment(Post.data, Comment.data, Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "Invalid or missing API key.").

test('Remove comment, on other user post', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    new_user(_{ username: 'author@example.com' }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    remove_comment(Post.data, Comment.data, Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "The operation requires access privileges.").

test('Get random human question', [setup(new_database)]):-
    set_no_auth,
    get_question(Question),
    assertion(Question.status = "success"),
    assertion(is_dict(Question.data)),
    Question.data = Data,
    assertion(string(Data.question)),
    assertion(number(Data.id)).

test('Entry author notification', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    assertion(bc_mail_queue_size(1)).

test('Entry author notifications disabled', [setup(new_database)]):-
    default_user_id(AuthorId),
    ds_update(AuthorId, _{ comment_notifications: false }),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data, Comment),
    assertion(Comment.status = "success"),
    assertion(bc_mail_queue_size(0)).

test('Mention notification, no receivers', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data, Comment1),
    assertion(Comment1.status = "success"),
    new_comment(Post.data,
        _{ content: "@RLa, reply" }, Comment2),
    assertion(Comment2.status = "success").

test('Mention notification, one receiver', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data,
        _{ notify: true, email: 'test@example.com' }, Comment1),
    assertion(Comment1.status = "success"),
    new_comment(Post.data,
        _{ content: "@RLa, reply" }, Comment2),
    assertion(Comment2.status = "success"),
    assertion(bc_mail_queue_size(3)).

test('Mention notification, two receivers', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data,
        _{ notify: true, email: 'test@example.com' }, Comment1),
    assertion(Comment1.status = "success"),
    new_comment(Post.data,
        _{ author: "User", notify: true, email: 'test@example.com' }, Comment2),
    assertion(Comment2.status = "success"),
    new_comment(Post.data,
        _{ content: "@RLa, @User, reply" }, Comment3),
    assertion(Comment3.status = "success"),
    assertion(bc_mail_queue_size(5)).

test('Reply notification', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data,
        _{ notify: true, email: 'test@example.com' }, Comment1),
    assertion(Comment1.status = "success"),
    new_comment(Post.data, _{
        content: "Reply",
        reply_to: Comment1.data }, Comment2),
    assertion(Comment2.status = "success"),
    assertion(bc_mail_queue_size(3)).

test('Mail unsubscribe from entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data,
        _{ notify: true, email: 'test@example.com' }, Comment1),
    assertion(Comment1.status = "success"),
    new_comment(Post.data,
        _{ content: "@RLa, reply" }, Comment2),
    assertion(Comment2.status = "success"),
    assertion(bc_mail_queue_size(3)),
    atom_concat('http://localhost:18008/mail/unsubscribe/entry/',
        Comment1.data, UnsubscribeURL),
    http_open(UnsubscribeURL, Stream, []),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    string_codes(Message, Codes),
    assertion(sub_string(Message, _, _, _, "unsubscribed")),
    new_comment(Post.data,
        _{ content: "@RLa, reply" }, Comment3),
    assertion(Comment3.status = "success"),
    assertion(bc_mail_queue_size(4)).

test('Mail unsubscribe from all', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    new_comment(Post.data,
        _{ notify: true, email: 'test@example.com' }, Comment1),
    assertion(Comment1.status = "success"),
    new_comment(Post.data,
        _{ content: "@RLa, reply" }, Comment2),
    assertion(Comment2.status = "success"),
    assertion(bc_mail_queue_size(3)),
    atom_concat('http://localhost:18008/mail/unsubscribe/all/',
        Comment1.data, UnsubscribeURL),
    http_open(UnsubscribeURL, Stream, []),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    string_codes(Message, Codes),
    assertion(sub_string(Message, _, _, _, "unsubscribed")),
    new_comment(Post.data,
        _{ content: "@RLa, reply" }, Comment3),
    assertion(Comment3.status = "success"),
    assertion(bc_mail_queue_size(4)).

:- end_tests(api_comment).
