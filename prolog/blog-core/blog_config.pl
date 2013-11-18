:- module(blog_config, [
    config_get/2,
    config_set/2
]).

/** <module> Configuration interface.
*/

:- use_module(blog_prop).
:- use_module(library(docstore)).

%% config_get(+Name, -Value) is det.
%
% Retrieves the given configuration
% value. Throws error(no_config(Name)) when
% the configuration option is not found.

config_get(Name, Value):-
    find(config, name=Name, [Doc]), !,
    prop_get(value, Doc, Value).
    
config_get(Name, _):-
    throw(error(no_config(Name))).
    
%% config_set(+Name, +Value) is det.
%
% Sets the configuration value. If the
% value does not exist yet, it is added.

config_set(Name, Value):-
    find(config, name=Name, [Doc]), !,
    memberchk('$id'(Id), Doc),
    prop_update(Id, value, Value).

config_set(Name, Value):-
    insert(config, [name(Name), value(Value)]).
