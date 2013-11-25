:- module(bc_init, [
    bc_setup/1,  % +File,
    bc_profile/1 % +File
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(threadutil)).

:- use_module(bc_config).
:- use_module(bc_admin).
:- use_module(bc_data).
:- use_module(bc_router).

%% bc_setup(+File) is det.
%
% Opens the database, inserts
% initial data (when it is missing)
% and starts the server.

bc_setup(File):-
    bc_data_init(File),
    start_server.

start_server:-
    config_get(server_port, Port),
    config_get(num_workers, Workers),
    config_get(timeout_worker, Timeout),
    http_server(bc_route, [
        port(Port),
        workers(Workers),
        timeout(Timeout)
    ]).

%% bc_profile(+File) is det.
%
% Opens database and starts server
% with a single thread. Enables profiling
% on the thread.
    
bc_profile(File):-
    bc_data_init(File),
    config_get(server_port, Port),
    http_server(bc_route, [
        port(Port),
        workers(1)
    ]),
    http_current_worker(Port, Worker),
    tprofile(Worker).
