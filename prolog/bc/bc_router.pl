:- module(bc_router, [
    bc_route/1,       % +Request
    bc_rewrite_hook/1 % :Goal
]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(debug)).
:- use_module(library(ar_router)).

:- module_transparent(bc_rewrite_hook/1).

:- dynamic(rewrite_hook/1).

%% bc_rewrite_hook(:Goal) is det.
%
% Registers new request path rewrite
% hook. These are executed before
% dispatching.

bc_rewrite_hook(Module:Goal):- !,
    (   rewrite_hook(Module:Goal)
    ->  true
    ;   asserta(bc_router:rewrite_hook(Module:Goal))).
    
bc_rewrite_hook(Goal):-
    context_module(Module),
    bc_rewrite_hook(Module:Goal).
    
run_rewrite_hooks(PathIn, PathOut):-
    findall(Hook, rewrite_hook(Hook), Hooks),
    run_rewrite_hooks(Hooks, PathIn, PathOut).
    
run_rewrite_hooks([Hook|Hooks], PathIn, PathOut):-
    (   call(Hook, PathIn, Tmp)
    ->  run_rewrite_hooks(Hooks, Tmp, PathOut)
    ;   run_rewrite_hooks(Hooks, PathIn, PathOut)).
    
run_rewrite_hooks([], Path, Path).

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
    select(path(Path), Request, NoPathRequest),
    debug(bc_router, 'routing ~p', [Path]),
    run_rewrite_hooks(Path, RewrPath),
    RewrRequest = [path(RewrPath)|NoPathRequest],
    debug(bc_router, 'dispatching ~p', [RewrPath]),
    (   try_route(RewrRequest)
    ->  true
    ;   (   serve_file(RewrRequest)
        ->  true
        ;   http_dispatch(RewrRequest))).

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
