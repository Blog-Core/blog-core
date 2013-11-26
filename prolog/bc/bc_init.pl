:- module(bc_init, [
    bc_setup/1,  % +Options,
    bc_profile/1 % +Options
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(threadutil)).
:- use_module(library(option)).

:- use_module(bc_config).
:- use_module(bc_admin).
:- use_module(bc_data).
:- use_module(bc_router).

%% database(-File) is semidet.
%
% Specifies the docstore
% database location.

:- multifile(database/1).

init_docstore(Options):-
    (   memberchk(file(File), Options)
    ->  bc_data_init(File)
    ;   (   database(File)
        ->  bc_data_init(File)
        ;   throw(error(database_not_specified)))).

%% bc_setup(+Options) is det.
%
% Opens the database, inserts
% initial data (when it is missing)
% and starts the server.
%
% Throws error(database_not_specified) when
% the database file is not set.

bc_setup(Options):-
    init_docstore(Options),
    start_server(Options).

start_server(Options):-
    http_server(bc_route, Options).

%% bc_profile(+Options) is det.
%
% Opens database and starts server
% with a single thread. Enables profiling
% on the thread.
    
bc_profile(Options):-
    init_docstore(Options),
    option(port(Port), Options, 8080),
    http_server(bc_route, [port(Port), workers(1)]),
    http_current_worker(Port, Worker),
    tprofile(Worker).
