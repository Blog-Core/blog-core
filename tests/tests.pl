:- use_module(prolog/bc/bc_daemon).
:- bc_daemon('site.docstore').

:- load_files([
    tests/api
], [ if(not_loaded) ]).
