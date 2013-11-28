:- module(bc_daemon, [
    bc_init_daemon
]).

:- use_module(library(http/http_unix_daemon)).
:- use_module(bc_init).

% Flag to prevent initialization
% multiple times.

:- dynamic(initialized).

%% bc_init_daemon is det.
%
% Daemonizes the currently running
% server by calling http_daemon/1.

bc_init_daemon:-
    (   initialized
    ->  true
    ;   http_daemon,
        assertz(initialized)).

% Sets up the hook that calls bc_setup/1.

http_unix_daemon:http_server_hook(Options):-
    bc_setup(Options).
