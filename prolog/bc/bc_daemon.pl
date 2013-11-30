:- module(bc_daemon, [
    bc_daemon
]).

:- use_module(library(http/http_unix_daemon)).
:- use_module(bc_main).

% Flag to prevent initialization
% multiple times.

:- dynamic(initialized).

%% bc_daemon is det.
%
% Daemonizes the currently running
% server by calling http_daemon/1.

bc_daemon:-
    (   initialized
    ->  true
    ;   http_daemon,
        assertz(initialized)).

% Sets up the hook that calls bc_setup/1.

http_unix_daemon:http_server_hook(Options):-
    bc_start(Options).
