:- begin_tests(api_entry).

:- use_module(library(docstore)).

:- use_module(util/util).
:- use_module(util/util_post).
:- use_module(util/util_user).
:- use_module(util/util_comment).

test('New entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success").

test('New entry, invalid data', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, _{ title: "" }, Post),
    assertion(is_invalid_data(Post)).

test('New entry, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    set_no_auth,
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "error"),
    assertion(Post.message = "Invalid or missing API key.").

test('New entry, duplicate slug', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post1),
    assertion(Post1.status = "success"),
    new_post(AuthorId, test_post, Post2),
    assertion(Post2.status = "error"),
    assertion(Post2.message = "The entry with the same slug exists already.").

% FIXME test case for custom content type.

test('Update entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    update_post(Post.data, _{ content: "**test 1**" }, Updated),
    assertion(Updated.status = "success").

test('Update entry, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    update_post(Post.data, _{}, Updated),
    assertion(Updated.status = "error"),
    assertion(Updated.message = "Invalid or missing API key.").

test('Update non-existent entry', [setup(new_database)]):-
    update_post('xxx-entry-not-exist', _{}, Updated),
    assertion(Updated.status = "error"),
    assertion(Updated.message = "The entry does not exist.").

test('Update entry, duplicate slug', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post_1, Post1),
    assertion(Post1.status = "success"),
    new_post(AuthorId, test_post_2, Post2),
    assertion(Post2.status = "success"),
    update_post(Post2.data, _{ slug: test_post_1 }, Updated),
    assertion(Updated.status = "error"),
    assertion(Updated.message = "The entry with the same slug exists already.").

test('Update entry, not own post', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_user(_{ username: 'author@example.com' }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    update_post(Post.data, _{}, Updated),
    assertion(Updated.status = "error"),
    assertion(Updated.message = "The operation requires access privileges.").

test('Update entry, author to other author', [setup(new_database)]):-
    new_user(_{ username: 'author1@example.com', type: author }, User1),
    assertion(User1.status = "success"),
    new_user(_{ username: 'author2@example.com', type: author }, User2),
    assertion(User2.status = "success"),
    set_default_username('author1@example.com'),
    new_post(User1.data, test_post, Post),
    assertion(Post.status = "success"),
    update_post(Post.data, _{ author: User2.data }, Updated),
    assertion(Updated.status = "success").

test('Update entry, admin to other author', [setup(new_database)]):-
    new_user(_{ username: 'author@example.com', type: author }, User),
    assertion(User.status = "success"),
    new_post(User.data, test_post, Post),
    assertion(Post.status = "success"),
    update_post(Post.data, _{ author: User.data }, Updated),
    assertion(Updated.status = "success").

test('List of posts', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    list_posts(Posts),
    assertion(Posts.status = "success"),
    assertion(is_list(Posts.data)),
    Posts.data = [First|_],
    assertion(get_dict(title, First, _)),
    assertion(get_dict(author, First, _)),
    assertion(get_dict(date_published, First, _)),
    assertion(get_dict(date_updated, First, _)).

test('List of posts, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    list_posts(Posts),
    assertion(Posts.status = "error"),
    assertion(Posts.message = "Invalid or missing API key.").

test('Get single entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    get_entry(Post.data, PostGet),
    assertion(PostGet.status = "success"),
    PostGet.data = Data,
    assertion(get_dict(title, Data, _)),
    assertion(get_dict(author, Data, _)),
    assertion(get_dict(date_published, Data, _)),
    assertion(get_dict(date_updated, Data, _)),
    assertion(get_dict(content, Data, _)),
    assertion(get_dict(comments, Data, _)).

test('Get single non-existent entry', [setup(new_database)]):-
    get_entry('xxx-entry-not-exists', Entry),
    assertion(Entry.status = "error"),
    assertion(Entry.message = "The entry does not exist.").

test('Get single entry, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    get_entry(Post.data, PostGet),
    assertion(PostGet.status = "error"),
    assertion(PostGet.message = "Invalid or missing API key.").

test('Get single entry info', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    get_entry_info(Post.data, PostGet),
    assertion(PostGet.status = "success"),
    PostGet.data = Data,
    assertion(\+ get_dict(content, Data, _)).

test('Get single entry info, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    get_entry_info(Post.data, PostGet),
    assertion(PostGet.status = "error"),
    assertion(PostGet.message = "Invalid or missing API key.").

test('Remove entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    remove_post(Post.data, Removed),
    assertion(Removed.status = "success").

test('Remove entry, no authentication', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    set_no_auth,
    remove_post(Post.data, Removed),
    assertion(Removed.status = "error"),
    assertion(Removed.message = "Invalid or missing API key.").

test('Remove entry, not own post', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    new_user(_{ username: 'author@example.com', type: author }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    remove_post(Post.data, Removed),
    assertion(Removed.status = "error"),
    assertion(Removed.message = "The operation requires access privileges.").

test('Remove non-existent entry', [setup(new_database)]):-
    remove_post('xxx-entry-not-exists', Entry),
    assertion(Entry.status = "error"),
    assertion(Entry.message = "The entry does not exist.").

:- end_tests(api_entry).
