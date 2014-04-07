:- module(bc_data_config, [
    bc_config_get/2, % +Name, -Value
    bc_config_set/2, % +Name, +Value
    bc_config_list/1 % -List
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
    ->  get_dict_ex(value, Doc, Value)
    ;   throw(error(no_config(Name)))).

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

%! bc_config_list(-List) is det.
%
% Retrieves the list of all config
% values. Returned list contains dicts
% `config{ name: Name, value: Value }`.

bc_config_list(List):-
    ds_all(config, List).
