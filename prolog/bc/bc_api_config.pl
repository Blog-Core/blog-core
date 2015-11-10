:- module(bc_api_config, []).

/** <module> HTTP handlers for config entry management */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_view).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_data_config).

% Gets config values.

:- route_get(api/configs,
    bc_auth, config_list).

config_list:-
    bc_actor(Actor),
    bc_config_list(Actor, List),
    bc_reply_success(List).

% Updates the config value.

:- route_put(api/config,
    bc_auth, config_update).

config_update:-
    bc_read_by_schema(bc_config, Config),
    Name = Config.name,
    Value = Config.value,
    bc_actor(Actor),
    bc_config_set_api(Actor, Name, Value),
    bc_view_purge_cache,
    bc_reply_success(Name).

% Generic config entry.

:- register_schema(bc_config, _{
    type: dict,
    tag: config,
    keys: _{
        name: atom,
        value: [ atom, number ]
    }
}).
