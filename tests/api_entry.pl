:- begin_tests(api_entry).

:- use_module(library(docstore)).

:- use_module(util).
:- use_module(util_post).
:- use_module(util_comment).

test('New entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success").

test('Update entry', [setup(new_database)]):-
    default_user_id(AuthorId),
    new_post(AuthorId, test_post, Post),
    assertion(Post.status = "success"),
    update_post(Post.data, _{ content: "**test 1**" }, Updated),
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
    assertion(get_dict(content, Data, _)).

% FIXME try edit one without permission

:- end_tests(api_entry).
