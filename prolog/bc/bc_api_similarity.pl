:- module(bc_api_similarity, []).

/** <module> Similarity API handlers */

:- use_module(library(arouter)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_similarity).

:- route_get(api/similar/Type/(public)/Id,
    similar_entries(Type, Id)).

similar_entries(Type, Id):-
    bc_similar(Type, Id, Entries),
    bc_reply_success(Entries).
