:- module(bc_api_search, []).

/** <module> Search API handlers */

:- use_module(library(arouter)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).

:- use_module(bc_api_io).
:- use_module(bc_search).

:- route_get(api/search/Type, search(Type)).

search(Type):-
    http_current_request(Request),
    http_parameters(Request, [
        q(Query, [atom, default('')])]),
    bc_search(Type, Query, Results),
    bc_reply_success(Results).
