:- module(bc_main, [
    bc_start/1
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(debug)).

:- use_module(bc_data).
:- use_module(bc_router).
:- use_module(bc_admin).

%% bc_start(+Options) is det.
%
% Starts the HTTP server. Options are passed
% to http_server/2 as-is.

bc_start(Options):-
    debug(bc_daemon, 'starting to serve HTTP, ~p', [Options]),
    http_server(bc_route, Options).
