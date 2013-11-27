:- module(bc_daemon, [
    bc_init_daemon
]).

:- use_module(library(http/http_unix_daemon)).

:- use_module(bc_init).

bc_init_daemon:-
    http_daemon.

% Sets up the hook that calls bc_setup/1.

http_unix_daemon:http_server_hook(Options):-
    bc_setup(Options).
