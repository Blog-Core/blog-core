:- module(bc_data, [
    bc_data_open/1, % +File
    bc_data_close/0
]).

:- use_module(library(docstore)).
:- use_module(library(debug)).

:- use_module(bc_data_config).
:- use_module(bc_data_user).

%! bc_data_open(+File) is det.
%
% Opens the docstore database file.
% Inserts the initial data.

bc_data_open(File):-
    ds_open(File),
    bc_init,
    debug(bc_data, 'opened docstore file ~p', [File]).

%! bc_data_close is det.
%
% Closes the docstore database.

bc_data_close:-
    ds_close,
    debug(bc_data, 'closed docstore file', []).

% Sets up initial values.
% Inserts the default admin user.

bc_init:-
    ds_all(config, []), !,
    debug(bc_data, 'inserting initial data', []),
    bc_config_set(title, 'Untitled site'),
    bc_user_save(user{
        fullname: 'Admin',
        username: 'admin',
        password: 'admin',
        type: admin
    }, _).

bc_init.
