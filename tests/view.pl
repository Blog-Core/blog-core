:- begin_tests(view).

:- use_module(library(arouter)).
:- use_module(library(docstore)).
:- use_module(prolog/bc/bc_view).
:- use_module(prolog/bc/bc_data_entry).
:- use_module(util).

% Test handler for viewing a single post.

:- route_get(post/Slug, view_post(Slug)).

view_post(Slug):-
    ds_find(entry, slug=Slug, [Post]),
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
    ds_find(entry, slug='default-test-post', [Post]),
    Post.'$id' = Id,
    Post.author = Author,
    atom_concat('/api/entry/', Id, Path),
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
    Update.status = "success",
    request_get_content('/post/default-test-post', Html2),
    sub_string(Html2, _, _, _, "<strong>modified</strong>"), !.

% FIXME test case for custom content type.

:- end_tests(view).
