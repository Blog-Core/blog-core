:- module(bc_daemon, [
    bc_daemon/1
]).

:- use_module(library(http/http_unix_daemon)).
:- use_module(library(docstore)).
:- use_module(bc_main).

% Flag to prevent initialization
% multiple times.

:- dynamic(initialized).

% Stores the docstore file name.

:- dynamic(docstore_file/1).

%% bc_daemon(File) is det.
%
% Daemonizes the currently running
% server by calling http_daemon/1.

bc_daemon(File):-
    (   initialized
    ->  true
    ;   http_daemon,
        assertz(docstore_file(File)),
        assertz(initialized)).

% Sets up the hook that opens the database
% and calls bc_start/1.

http_unix_daemon:http_server_hook(Options):-
    docstore_file(File),
    ds_open(File),
    bc_start(Options).
