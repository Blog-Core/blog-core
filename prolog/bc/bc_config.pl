:- module(bc_config, [
    config_get/2, % +Name, -Value
    config_set/2  % +Name, +Value
]).

/** <module> Configuration interface.
*/

:- use_module(library(docstore)).
:- use_module(library(debug)).

%! config_get(+Name, -Value) is det.
%
% Retrieves the given configuration
% value. Throws error(no_config(Name)) when
% the configuration option is not found.

config_get(Name, Value):-
    (   ds_find(config, name=Name, [Doc])
    ->  get_dict_ex(value, Doc, Value)
    ;   throw(error(no_config(Name)))).

%! config_set(+Name, +Value) is det.
%
% Sets the configuration value. If the
% value does not exist yet, it is added.

config_set(Name, Value):-
    debug(bc_config, 'setting ~w to ~p', [Name, Value]),
    (   ds_find(config, name=Name, [Doc])
    ->  put_dict(value, Doc, Value, New),
        ds_update(New)
    ;   ds_insert(config{ name: Name, value: Value })).
