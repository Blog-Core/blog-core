:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(arouter)).

:- use_module(prolog/bc/bc_main).
:- use_module(prolog/bc/bc_data).

:- bc_main('test.docstore', [port(18008), workers(8)]).

:- load_files([
    tests/api_config,
    tests/api_user,
    tests/api_entry,
    tests/api_comment,
    tests/files,
    tests/view
], [ if(not_loaded) ]).

% Resets database state. Used
% for admin tests.

:- route_get(reset, reset).

reset:-
    bc_data_close,
    (   exists_file('test.docstore')
    ->  delete_file('test.docstore')
    ;   true),
    bc_data_open('test.docstore'),
    write('Content-type: text/plain; charset=UTF-8\r\n\r\n'),
    write('OK').
