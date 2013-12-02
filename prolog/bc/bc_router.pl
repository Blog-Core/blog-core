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
%
% When ar_route/1 fails and path has
% slash at end then HTTP redirect 301
% is sent (location canonicalization).
%
% TODO make canonicalization optional?

bc_route(Request):-
    (   try_route(Request)
    ->  true
    ;   (   serve_file(Request)
        ->  true
        ;   http_dispatch(Request))).
        
try_route(Request):-
    (   ar_route(Request)
    ->  true
    ;   memberchk(path(Path), Request),
        atom_concat(Prefix, '/', Path),
        Prefix \= '',
        http_redirect(moved, Prefix, Request)).

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
