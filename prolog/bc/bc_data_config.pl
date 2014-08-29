:- module(bc_data_config, [
    bc_config_get/2,     % +Name, -Value
    bc_config_set/2,     % +Name, +Value
    bc_config_set_api/2, % +Name, +Value
    bc_config_list/1     % -List
]).

:- use_module(library(debug)).
:- use_module(library(docstore)).

:- use_module(bc_data_cur_user).

%! bc_config_get(+Name, -Value) is det.
%
% Retrieves the configuration entry.
% When the entry does not exist then
% an error error(no_config(Name)) is thrown.

bc_config_get(Name, Value):-
    (   ds_find(config, name=Name, [Doc])
    ->  Value = Doc.value
    ;   throw(error(no_config(Name)))).

%! bc_config_set_api(+Name, +Value) is det.
%
% Same as bc_config_get/2 but checks that
% the current API user is an admin.

bc_config_set_api(Name, Value):-
    check_current_user_is_admin,
    bc_config_set(Name, Value).

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
    check_current_user_is_admin,
    ds_all(config, List).

% FIXME consolidate to single file.

check_current_user_is_admin:-
    (   bc_user(User), User.type = admin
    ->  true
    ;   throw(error(user_current_is_not_admin))).
