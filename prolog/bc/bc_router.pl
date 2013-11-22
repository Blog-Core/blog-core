:- module(bc_router, [
    bc_route/1 % +Request
]). 

:- use_module(library(http/http_dispatch)).
:- use_module(library(debug)).
:- use_module(library(ar_router)).

%% bc_route(+Request) is det.
%
% Routes the given HTTP request.
% First tries ar_route/1. If it fails
% then it tries serve_file/1. Finally
% tries http_dispatch/1.

bc_route(Request):-
    (   ar_route(Request)
    ->  true
    ;   (   serve_file(Request)
        ->  true
        ;   http_dispatch(Request))).

%% serve_file(+Request) is semidet.
%
% Tries to serve matching file from
% the public folder. If it does not exist,
% the predicate fails.
        
serve_file(Request):-
    memberchk(path(Path), Request),
    atom_concat(public, Path, File),
    exists_file(File),
    http_reply_file(File, [], Request).
