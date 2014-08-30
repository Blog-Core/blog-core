:- begin_tests(view).

:- use_module(library(arouter)).
:- use_module(library(docstore)).
:- use_module(prolog/bc/bc_view).
:- use_module(prolog/bc/bc_data_entry).

:- use_module(util/util).
:- use_module(util/util_post).

% Test handler for viewing a single post.

:- route_get(post/Slug, view_post(Slug)).

view_post(Slug):-
    ds_find(entry, slug=Slug, [Post]),
    bc_view_send(tests/views/post, Post).

test('Entry without cache', [setup((bc_view_disable_cache, new_database))]):-
    default_user_id(AuthorId),
    new_post(AuthorId, 'view-test', Post),
    assertion(Post.status = "success"),
    request_get_content('/post/view-test', Html),
    assertion(sub_string(Html, _, _, _, "<strong>test</strong>")), !.

test('Entry with cache', [setup((bc_view_enable_cache, new_database))]):-
    default_user_id(AuthorId),
    new_post(AuthorId, 'view-test', Post),
    assertion(Post.status = "success"),
    request_get_content('/post/view-test', Html1),
    assertion(sub_string(Html1, _, _, _, "<strong>test</strong>")), !,
    request_get_content('/post/view-test', Html2),
    assertion(sub_string(Html2, _, _, _, "<strong>test</strong>")), !.

test('Cache purge', [setup((bc_view_enable_cache, new_database))]):-
    default_user_id(AuthorId),
    new_post(AuthorId, 'view-test', Post),
    assertion(Post.status = "success"),
    bc_view_purge_cache,
    request_get_content('/post/view-test', Html1),
    assertion(sub_string(Html1, _, _, _, "<strong>test</strong>")),
    update_post(Post.data, _{ content: "**modified**", slug: 'view-test' }, Update),
    assertion(Update.status = "success"),
    request_get_content('/post/view-test', Html2),
    assertion(sub_string(Html2, _, _, _, "<strong>modified</strong>")), !.

:- end_tests(view).
