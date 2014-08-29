:- use_module(prolog/bc/bc_main).
:- bc_main('blog.docstore', [port(18008)]).

:- load_files([
    tests/api_user,
    tests/api_entry
    %tests/api_comment
    %tests/api_comment_reply_to,
    %tests/files,
    %tests/view
], [ if(not_loaded) ]).
