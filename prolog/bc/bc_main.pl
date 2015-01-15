:- module(bc_main, [
    bc_main/1,
    bc_main/2,
    bc_environment/1 % -Env
]).

% Catch uncaught errors/warnings and shut down
% when they occur.

:- if(getenv('PL_ENV', production)).
    user:message_hook(Term, Type, _):-
        ( Type = error ; Type = warning ),
        message_to_string(Term, String),
        write(user_error, String), nl(user_error),
        halt(1).
:- endif.

:- set_prolog_flag(encoding, utf8).

:- use_module(library(dcg/basics)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(debug)).
:- use_module(library(docstore)).
:- use_module(library(arouter)).
:- use_module(library(st/st_expr)).
:- use_module(library(st/st_file)).
:- use_module(library(st/st_parse)).

:- use_module(bc_api).
:- use_module(bc_router).
:- use_module(bc_bust).
:- use_module(bc_view).
:- use_module(bc_admin).
:- use_module(bc_excerpt).
:- use_module(bc_data).
:- use_module(bc_data_migrate).

%! bc_environment(-Env) is det.
%
% Queries the current environment.
% Env is either an atom `production` or `development`.
% The environment is determined by the PL_ENV environment
% variable. All values other than production will enable
% the development environment.

:- dynamic(bc_environment/1).

% In development: most debug features.
% In production: enable simple-template and view caching.

:- if(getenv('PL_ENV', production)).
    :- asserta(bc_environment(production)).
    :- st_enable_cache.
    :- bc_view_enable_cache.
:- else.
    :- asserta(bc_environment(development)).
    :- write(user_error, 'Running in development mode!'), nl(user_error).
    :- use_module(library(http/http_error)).
    %:- debug(http(_)).
    :- debug(arouter).
    :- debug(docstore).
    :- debug(bc_data).
    :- debug(bc_data_migrate).
    :- debug(bc_data_comment).
    :- debug(bc_router).
    :- debug(bc_view).
    :- debug(bc_bust).
:- endif.

% Sets up simple-template.

:- st_enable_strip_white.
:- st_set_extension(html).
:- st_set_function(excerpt, 2, bc_excerpt).

% When platform is not Windows then it assumed that
% http_unix_daemon is supported.

:- if(not(current_prolog_flag(windows, true))).
    :- use_module(library(http/http_unix_daemon)).
    http_unix_daemon:http_server_hook(Options):-
        http_server(bc_route, Options).
:- endif.

:- dynamic(initialized).

%! bc_main(+File) is det.
%
% Opens docstore database and runs the
% frameworks setup code.

bc_main(_):-
    initialized, !.

bc_main(File):-
    (   current_prolog_flag(windows, true)
    ->  write(user_error, 'http_unix_daemon daemon is not supported on Windows'), nl(user_error),
        write(user_error, 'shim supporting the --port option is used'), nl(user_error),
        port_option(Port),
        bc_main(File, [port(Port)])
    ;   bc_data_open(File),
        http_daemon,
        asserta(initialized)).

:- setting(port, number, 80, 'Port to run on in windows').

:- load_settings('settings.db').

port_option(Port):-
    current_prolog_flag(argv, Argv),
    (   find_port_option(Argv, Port)
    ->  true
    ;   setting(port, Port)).

find_port_option([Arg|Argv], Port):-
    atom_codes(Arg, Codes),
    (   phrase(port_option_parse(Port), Codes)
    ->  true
    ;   find_port_option(Argv, Port)).

port_option_parse(Port) -->
    "--port=", integer(Port), { Port > 0 }.

%! bc_main(+File, +Options) is det.
%
% Same as bc_main/1 but does not use
% http_unix_daemon. Options are passed
% to http_server/2.

bc_main(_, _):-
    initialized, !.

bc_main(File, Options):-
    bc_data_open(File),
    http_server(bc_route, Options),
    asserta(initialized).
