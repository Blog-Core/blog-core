:- begin_tests(api_config).

:- use_module(util/util).
:- use_module(util/util_config).
:- use_module(util/util_user).

test('List of entries', [setup(new_database)]):-
    list_configs(List),
    assertion(List.status = "success"),
    assertion(is_list(List.data)),
    List.data = [ Item | _ ],
    assertion(get_dict(name, Item, _)),
    assertion(get_dict(value, Item, _)).

test('List of entries, no authentication', [setup(new_database)]):-
    set_no_auth,
    list_configs(List),
    assertion(List.status = "error"),
    assertion(List.message = "Invalid or missing API key.").

test('List of entries, no admin', [setup(new_database)]):-
    new_user(_{ type: author, username: 'author@example.com' }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    list_configs(List),
    assertion(List.status = "error"),
    assertion(List.message = "The operation requires access privileges.").

test('Update entry', [setup(new_database)]):-
    update_config(title, "New title", Config),
    assertion(Config.status = "success").

test('Update entry, no authentication', [setup(new_database)]):-
    set_no_auth,
    update_config(title, "New title", Config),
    assertion(Config.status = "error"),
    assertion(Config.message = "Invalid or missing API key.").

test('Update entry, no admin', [setup(new_database)]):-
    new_user(_{ type: author, username: 'author@example.com' }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    update_config(title, "New title", Config),
    assertion(Config.status = "error"),
    assertion(Config.message = "The operation requires access privileges.").

test('Add entry', [setup(new_database)]):-
    update_config(not_used_before, 123, Config),
    assertion(Config.status = "success"),
    list_configs(List),
    assertion(List.status = "success"),
    assertion(is_list(List.data)),
    List.data = Data, !,
    member(Item, Data),
    get_dict(name, Item, "not_used_before"),
    get_dict(value, Item, 123).

:- end_tests(api_config).
