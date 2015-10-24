:- module(bc_router, [
    bc_route/1,         % +Request
    bc_enable_expires/0
]).

/** <module> HTTP routing

Top-level HTTP handler that provides file
serving, routing through arouter and
fallback to http_dispatch/1.
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(debug)).
:- use_module(library(arouter)).

:- use_module(bc_view).
:- use_module(bc_headers).

:- dynamic(expires/0).

%! bc_enable_expires is det.
%
% Enables Cache-Control and
% Expires headers.

bc_enable_expires:-
    (   expires
    ->  true
    ;   asserta(expires)),
    debug(bc_route, 'static file Expires/Cache-Control enabled', []).

% Time in seconds for files
% to be cached by clients.

cache_control(5184000). % 60 days

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
    (   expires,
        memberchk(cache_token(true), Request)
    ->  cache_control(MaxAge),
        get_time(Time),
        Expires is Time + MaxAge,
        http_timestamp(Expires, ExpiresString), % TODO move to bc_headers
        format('Expires: ~w\r\n', [ExpiresString]),
        format('Cache-Control: max-age=~w\r\n', [MaxAge])
    ;   true),
    http_reply_file(File, [cache(true)], Request).
