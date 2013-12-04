:- module(bc_bust, [
    bc_bust_token/1 % -Atomic
]).

:- use_module(library(dcg/basics)).
:- use_module(bc_router).

%% bs_bust_token(-Token) is det.
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

:- bc_rewrite_hook(rewrite_cache_buster).

rewrite_cache_buster(In, Out):-
    atom_codes(In, InCodes),
    phrase(cache_busting_prefix, InCodes, Rest),
    atom_codes(Out, Rest).

cache_busting_prefix -->
    "/t-", integer(_).
