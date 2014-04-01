:- module(bc_bust, [
    bc_bust_token/1 % -Atomic
]).

/** <module> Client-side cache busting

This module provides client-side cache busting support
by rewriting request paths that contain specifically
formatted prefix. Adds `cache_token` global to
simple-template.
*/

:- use_module(library(http/http_wrapper)).
:- use_module(library(st/st_expr)).
:- use_module(library(dcg/basics)).
:- use_module(bc_router).

%! bc_bust_token(-Token) is det.
%
% Retrieves the token for cache
% busting. Currently it is set to
% the server start timestamp.

bc_bust_token(Token):-
    start_time(Token).

% Used for storing the server start time.
% Start time is used for cache busting.

:- dynamic(start_time/1).

set_start_time:-
    get_time(FloatTime),
    Time is round(FloatTime),
    retractall(start_time(_)),
    asserta(start_time(Time)).
    
:- set_start_time.

% Registers rewrite hook to remove
% the cache busting prefix.

http:request_expansion(RequestIn, RequestOut):-
    select(path(PathIn), RequestIn, Request),
    atom_codes(PathIn, CodesIn),
    phrase(cache_busting_prefix, CodesIn, OutCodes),
    atom_codes(PathOut, OutCodes),
    RequestOut = [path(PathOut)|Request],
    debug(bc_bust, 'Rewrote cache bust prefixed path ~p', [PathIn]).

cache_busting_prefix -->
    "/t-", integer(_).

% Add token value to simple-template
% global values.

:- bc_bust_token(Token),
    st_set_global(cache_token, Token).
