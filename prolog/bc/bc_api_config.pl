:- module(bc_api_config, []).

/** <module> HTTP handlers for config entry management */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_config).

% Gets config values.

:- route_get(api/configs,
    bc_auth, config_list).

config_list:-
    bc_config_list(List),
    bc_reply_success(List).

% Updates the config value.

:- route_put(api/config,
    bc_auth, config_update).

config_update:-
    bc_read_by_schema(config, Config),
    get_dict_ex(name, Config, Name),
    get_dict_ex(value, Config, Value),
    bc_config_set(Name, Value),
    bc_reply_success(Name).

% Generic config entry.

:- register_schema(config, _{
    type: dict,
    tag: config,
    keys: _{
        name: atom,
        value: [ atom, number ]
    }
}).
