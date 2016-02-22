:- module(bc_data_config, [
    bc_config_get/2,          % +Name, -Value
    bc_config_set/2,          % +Name, +Value
    bc_config_set_api/3,      % +Actor, +Name, +Value
    bc_config_set_list_api/2, % +Actor, +List
    bc_config_list/2,         % +Actor, -List
    bc_config_dict/1          % -Dict
]).

:- use_module(library(debug)).
:- use_module(library(docstore)).

%! bc_config_get(+Name, -Value) is det.
%
% Retrieves the configuration entry.
% When the entry does not exist then
% an error error(no_config(Name)) is thrown.

bc_config_get(Name, Value):-
    (   ds_find(config, name=Name, [Doc])
    ->  Value = Doc.value
    ;   throw(error(no_config(Name)))).

%! bc_config_set_api(+Actor, +Name, +Value) is det.
%
% Same as bc_config_set/2 but checks that
% the current API user is an admin.

bc_config_set_api(Actor, Name, Value):-
    config_access(Actor),
    bc_config_set(Name, Value).

%! bc_config_set_list_api(+Actor, List) is det.
%
% Same as bc_config_set_api/2 but takes
% a list of configuration entries.

bc_config_set_list_api(Actor, List):-
    config_access(Actor),
    maplist(config_set_dict, List).

config_set_dict(Dict):-
    bc_config_set(Dict.name, Dict.value).

config_access(Actor):-
    Actor.type = admin, !.

config_access(_):-
    throw(error(no_access)).

%! bc_config_set(+Name, +Value) is det.
%
% Sets the configuration value. If the
% value does not exist yet, it is added.

bc_config_set(Name, Value):-
    debug(bc_data, 'setting ~w to ~p', [Name, Value]),
    (   ds_find(config, name=Name, [Doc])
    ->  put_dict(value, Doc, Value, New),
        ds_update(New)
    ;   ds_insert(config{ name: Name, value: Value })).

%! bc_config_list(+Actor, -List) is det.
%
% Retrieves the list of all config
% values. Returned list contains dicts
% `config{ name: Name, value: Value }`.

bc_config_list(Actor, List):-
    config_access(Actor),
    ds_all(config, List).

%! bc_config_dict(-Dict) is det.
%
% Retrieves dict containing all
% config entries.

bc_config_dict(Dict):-
    ds_all(config, List),
    build_dict(List, _{}, Dict).

build_dict([Entry|List], Acc, Dict):-
    Tmp = Acc.put(Entry.name, Entry.value),
    build_dict(List, Tmp, Dict).

build_dict([], Dict, Dict).
