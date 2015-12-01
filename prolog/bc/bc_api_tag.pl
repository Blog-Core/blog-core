:- module(bc_api_tag, []).

/** <module> Tag API handlers */

:- use_module(library(arouter)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_tag_stat).

:- route_get(api/tags/Type/public,
    public_tags(Type)).

public_tags(Type):-
    bc_tag_stat(Type, Tags),
    bc_reply_success(Tags).

:- route_get(api/tags/Type/all,
    bc_auth, all_tags(Type)).

all_tags(Type):-
    bc_tag_stat_all(Type, Tags),
    bc_reply_success(Tags).
