:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(arouter)).

:- use_module(prolog/bc/bc_main).
:- use_module(prolog/bc/bc_data).
:- use_module(prolog/bc/bc_type).

:- bc_main('test.docstore', [port(18008), workers(8)]).

:- load_files([
    tests/api_config,
    tests/api_user,
    tests/api_entry,
    tests/api_comment,
    tests/api_search,
    tests/api_tag,
    tests/api_similarity,
    tests/files,
    tests/view,
    tests/mentions,
    tests/mail
], [ if(not_loaded) ]).

% Registers preview and canonical link for posts.

:- bc_register_preview(post, '/post/<slug>').
:- bc_register_canonical(post, '/post/<slug>').

% Resets database state. Used
% for admin tests.

:- route_get(reset, reset).

reset:-
    bc_data_close,
    (   exists_file('test.docstore')
    ->  delete_file('test.docstore')
    ;   true),
    bc_data_open('test.docstore'),
    (   exists_directory('public/new')
    ->  delete_directory('public/new')
    ;   true),
    (   exists_file('public/upload.txt')
    ->  delete_file('public/upload.txt')
    ;   true),
    (   exists_file('public/test.txt')
    ->  true
    ;   copy_file('tests/test.txt', 'public/test.txt')),
    (   exists_directory('public/path')
    ->  true
    ;   make_directory('public/path')),
    (   exists_directory('public/path/to')
    ->  true
    ;   make_directory('public/path/to')),
    (   exists_file('public/path/to/file.txt')
    ->  true
    ;   copy_file('tests/test.txt', 'public/path/to/file.txt')),
    write('Content-type: text/plain; charset=UTF-8\r\n\r\n'),
    write('OK').
