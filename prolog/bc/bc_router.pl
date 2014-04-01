:- module(bc_router, [
    bc_route/1 % +Request
]).

/** <module> HTTP routing

Top-level HTTP handler that provides file
serving, routing through arouter and
fallback to http_dispatch/1.
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(debug)).
:- use_module(library(arouter)).

:- use_module(bc_view).

%! bc_route(+Request) is det.
%
% Routes the given HTTP request.
% First tries ar_route/1. If it fails
% then it tries serve_file/1. Finally
% tries http_dispatch/1.

bc_route(Request):-    
    memberchk(path(Path), Request),
    debug(bc_router, 'Routing ~p', [Path]),
    (   try_route(Request)
    ;   (   serve_file(Request)
        ;   http_dispatch(Request))).

try_route(Request):-
    memberchk(path(Path), Request),
    (   bc_view_cached(Path)
    ;   (   route(Request)
        ;   atom_concat(Prefix, '/', Path),
            Prefix \= '',
            http_redirect(moved, Prefix, Request))).

%! serve_file(+Request) is semidet.
%
% Tries to serve matching file from
% the public folder. If it does not exist,
% the predicate fails.
        
serve_file(Request):-
    memberchk(path(Path), Request),
    atom_concat(public, Path, File),
    exists_file(File),
    http_reply_file(File, [], Request).
