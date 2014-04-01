:- use_module(prolog/bc/bc_main).
:- bc_main('test.docstore', [port(18008)]).

:- load_files([
    tests/api,
    tests/files,
    tests/view
], [ if(not_loaded) ]).
