:- module(bc_data, [
    bc_data_open/1, % +File
    bc_data_close/0
]).

:- use_module(library(docstore)).
:- use_module(library(debug)).

:- use_module(bc_data_config).
:- use_module(bc_data_user).
:- use_module(bc_migrate).
:- use_module(bc_search).
:- use_module(bc_type).
:- use_module(bc_role).

% Register initial roles.

:- bc_register_role(admin, 'Admin', true).
:- bc_register_role(author, 'Author', true).
:- bc_register_role(anon, 'Anonymous', false).

% Register initial types.

:- bc_register_type(post, 'Post', 'Posts', [
    admin(create, read_any, update_any, remove_any, publish_any, files),
    author(create, read_any, update_own, remove_own, publish_own, files)
], true).

:- bc_register_type(page, 'Page', 'Pages', [
    admin(create, read_any, update_any, remove_any, publish_any, files),
    author(create, read_any, update_own, remove_own, publish_own, files)
], false).

:- bc_register_type(block, 'Block', 'Blocks', [
    admin(create, read_any, update_any, remove_any, publish_any, files),
    author(create, read_any, update_own, remove_own, publish_own, files)
], false).

%! bc_data_open(+File) is det.
%
% Opens the docstore database file.
% Inserts the initial data.

bc_data_open(File):-
    ds_open(File),
    bc_init,
    bc_index_all,
    debug(bc_data, 'opened docstore file ~p', [File]).

%! bc_data_close is det.
%
% Closes the docstore database.

bc_data_close:-
    ds_close,
    bc_index_remove,
    debug(bc_data, 'closed docstore file', []).

% Sets up initial values.
% Inserts the default admin user.

bc_init:-
    bc_migrate(
        bc_initial_config,
        'Inserts the initial config',
        bc_initial_config),
    bc_migrate(
        bc_initial_user,
        'Inserts the initial user',
        bc_initial_user),
    bc_migrate(
        bc_add_language,
        'Adds language to posts',
        bc_add_language),
    bc_migrate(
        bc_smtp_settings,
        'Adds SMTP settings',
        bc_smtp_settings),
    bc_migrate(
        bc_remove_files,
        'Removes files key from users',
        bc_remove_files),
    bc_migrate(
        bc_smtp_security,
        'Adds smtp_security configuration parameter',
        bc_smtp_security),
    bc_migrate(
        bc_smtp_from,
        'Adds smtp_from configuration parameter',
        bc_smtp_from),
    bc_migrate(
        bc_comment_notifications,
        'Adds option for users to receive comment notifications',
        bc_comment_notifications).

% Inserts the initial config.

bc_initial_config:-
    bc_config_set(title, 'Untitled site').

bc_smtp_settings:-
    bc_config_set(smtp_enabled, false),
    bc_config_set(smtp_host, 'localhost'),
    bc_config_set(smtp_user, 'user'),
    bc_config_set(smtp_password, 'password'),
    bc_config_set(smtp_auth, 'login').

bc_smtp_security:-
    bc_config_set(smtp_security, 'none').

bc_smtp_from:-
    bc_config_set(smtp_from, 'admin@example.com').

bc_comment_notifications:-
    ds_col_add_key(user, comment_notifications, true).

% Inserts the initial user.

bc_initial_user:-
    bc_user_save_initial(user{
        fullname: 'Admin',
        username: 'admin@example.com',
        password: 'admin',
        type: admin,
        link: ""
    }).

% Adds and sets language for
% entries.

bc_add_language:-
    ds_col_add_key(entry, language, en),
    bc_config_set(default_language, en).

bc_remove_files:-
    ds_col_remove_key(user, files).
