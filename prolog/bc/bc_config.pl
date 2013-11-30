:- module(bc_config, [
    config_get/2, % +Name, -Value
    config_set/2  % +Name, +Value
]).

/** <module> Configuration interface.
*/

:- use_module(library(docstore)).
:- use_module(library(debug)).

:- use_module(bc_doc).

%% config_get(+Name, -Value) is det.
%
% Retrieves the given configuration
% value. Throws error(no_config(Name)) when
% the configuration option is not found.

config_get(Name, Value):-
    ds_find(config, name=Name, [Doc]), !,
    doc_get(value, Doc, Value).
    
config_get(Name, _):-
    throw(error(no_config(Name))).
    
%% config_set(+Name, +Value) is det.
%
% Sets the configuration value. If the
% value does not exist yet, it is added.

config_set(Name, Value):-
    ds_find(config, name=Name, [Doc]), !,
    debug(bc_config, 'setting ~w to ~p', [Name, Value]),
    doc_id(Doc, Id),
    ds_prop_update(Id, value, Value).

config_set(Name, Value):-
    debug(bc_config, 'setting ~w to ~p', [Name, Value]),
    ds_insert(config, [name(Name), value(Value)]).
