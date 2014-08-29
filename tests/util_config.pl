:- module(util_config, [
    list_configs/1,  % -Response
    update_config/3  % +Name, +Value, -Response
]).

:- use_module(util).

% Retrieves the list of configuration options.

list_configs(Response):-
    request_get('/api/configs', Response).

% Sets the given configuration option.

update_config(Name, Value, Response):-
    request_put('/api/config',
        _{ name: Name, value: Value }, Response).
