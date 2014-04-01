:- begin_tests(view).

:- use_module(library(arouter)).
:- use_module(prolog/bc/bc_view).
:- use_module(prolog/bc/bc_data).
:- use_module(util).

% Test handler for viewing a single post.

:- route_get(post/Slug, view_post(Slug)).

view_post(Slug):-
    bc_post_find_by_slug(Slug, Post),
    bc_view_send(tests/views/post, Post).

test(post, [setup((bc_view_disable_cache, new_database))]):-
    request_get_content('/post/default-test-post', Html),
    sub_string(Html, _, _, _, "<strong>test</strong>"), !.

test(post_cached, [setup((bc_view_enable_cache, new_database))]):-
    request_get_content('/post/default-test-post', Html1),
    sub_string(Html1, _, _, _, "<strong>test</strong>"), !,
    request_get_content('/post/default-test-post', Html2),
    sub_string(Html2, _, _, _, "<strong>test</strong>"), !.

test(cache_purge, [setup((bc_view_enable_cache, new_database))]):-
    bc_view_purge_cache,
    request_get_content('/post/default-test-post', Html1),
    sub_string(Html1, _, _, _, "<strong>test</strong>"), !,
    bc_post_find_by_slug('default-test-post', Post),
    get_dict_ex('$id', Post, Id),
    get_dict_ex(author, Post, Author),
    atom_concat('/api/post/', Id, Path),
    request_put(Path, _{
        author: Author,
        title: "Default Test post",
        slug: "default-test-post",
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**modified**",
        content_type: markdown,
        description: "Test",
        type: post
    }, Update),
    get_dict_ex(status, Update, "success"),
    request_get_content('/post/default-test-post', Html2),
    sub_string(Html2, _, _, _, "<strong>modified</strong>"), !.

:- end_tests(view).
