:- begin_tests(api_user).

:- use_module(library(docstore)).

:- use_module(util).
:- use_module(util_user).
:- use_module(util_post).

test('Authenticate', [setup(new_database)]):-
    authenticate_user('admin@example.com', admin, Auth),
    assertion(Auth.status = "success").

test('Authenticate, invalid credentials', [setup(new_database)]):-
    authenticate_user('admin@example.com', admin123, Auth),
    assertion(Auth.status = "error"),
    assertion(Auth.message = "Invalid auth credentials.").

test('Authenticate, invalid data', [setup(new_database)]):-
    authenticate_invalid(Auth),
    assertion(is_invalid_data(Auth)).

test('New user', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success").

test('New user, no authentication', [setup(new_database)]):-
    set_no_auth,
    new_user(_{}, User),
    writeln(User),
    assertion(User.status = "error"),
    assertion(User.message = "Invalid or missing API key.").

test('New user, username not email', [setup(new_database)]):-
    new_user(_{ username: test }, User),
    assertion(User.status = "error"),
    assertion(User.message = "The username is not an email address.").

test('New user, username empty', [setup(new_database)]):-
    new_user(_{ username: "" }, User),
    assertion(is_invalid_data(User)).

test('New user, fullname empty', [setup(new_database)]):-
    new_user(_{ fullname: "" }, User),
    assertion(is_invalid_data(User)).

test('New user, invalid type', [setup(new_database)]):-
    new_user(_{ type: invalid }, User),
    assertion(is_invalid_data(User)).

test('New user, files not boolean', [setup(new_database)]):-
    new_user(_{ files: 123 }, User),
    assertion(is_invalid_data(User)).

test('New user, username exists', [setup(new_database)]):-
    new_user(_{ username: 'exists@example.com' }, User1),
    assertion(User1.status = "success"),
    new_user(_{ username: 'exists@example.com' }, User2),
    assertion(User2.status = "error"),
    assertion(User2.message = "The username exists.").

test('New user with password not set', [setup(new_database)]):-
    new_user_no_password(User),
    assertion(User.status = "error"),
    assertion(User.message = "The user password is not set.").

test('New user, current not admin', [setup(new_database)]):-
    new_user(_{ username: 'noadmin@example.com', type: author }, User),
    assertion(User.status = "success"),
    set_default_username('noadmin@example.com'),
    new_user(_{ username: 'someone@example.com' }, Other),
    assertion(Other.status = "error"),
    assertion(Other.message = "The operation requires admin privileges.").

test('Get user', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success"),
    assertion(string(User.data)),
    get_user(User.data, GetUser),
    assertion(GetUser.status = "success"),
    assertion(is_dict(GetUser.data)),
    GetUser.data = Data,
    assertion(Data.files = true),
    assertion(Data.link = ""),
    assertion(Data.type = "author"),
    assertion(Data.username = "test@example.com"),
    assertion(\+ get_dict(key, Data, _)),
    assertion(\+ get_dict(salt, Data, _)),
    assertion(\+ get_dict(password, Data, _)).

test('Get user, no authentication', [setup(new_database)]):-
    default_user_id(UserId),
    set_no_auth,
    get_user(UserId, User),
    assertion(User.status = "error"),
    assertion(User.message = "Invalid or missing API key.").

test('Get non-existing user', [setup(new_database)]):-
    get_user('xxx-user-not-exists', User),
    assertion(User.status = "error"),
    assertion(User.message = "The user does not exist.").

test('Update user username', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success"),
    update_user(User.data, _{ username: 'updated@example.com' }, Update),
    assertion(Update.status = "success"),
    get_user(User.data, GetUser),
    assertion(GetUser.status = "success"),
    Data = GetUser.data,
    assertion(Data.username = "updated@example.com").

test('Update user, no authentication', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success"),
    set_no_auth,
    update_user(User.data, _{ username: 'updated@example.com' }, Update),
    assertion(Update.status = "error"),
    assertion(Update.message = "Invalid or missing API key.").

test('Update user username, current not admin', [setup(new_database)]):-
    new_user(_{ username: 'noadmin@example.com', type: author }, NoAdmin),
    assertion(NoAdmin.status = "success"),
    new_user(_{}, User),
    assertion(User.status = "success"),
    set_default_username('noadmin@example.com'),
    update_user(User.data, _{ username: 'updated@example.com' }, Update),
    assertion(Update.status = "error"),
    assertion(Update.message = "The operation requires admin privileges.").

test('Update user, demote the last admin', [setup(new_database)]):-
    default_user_id(UserId),
    update_user(UserId, _{ type: author }, Update),
    assertion(Update.status = "error"),
    assertion(Update.message = "Cannot demote the last admin.").

test('Update user, username to existing one', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success"),
    update_user(User.data, _{ username: 'admin@example.com' }, Update),
    assertion(Update.status = "error"),
    assertion(Update.message = "The username exists.").

test('Update non-existing user', [setup(new_database)]):-
    update_user('xxx-user-not-exists', _{}, Update),
    assertion(Update.status = "error"),
    assertion(Update.message = "The user does not exist.").

% FIXME ds_get choicepoint issue

test('Update user password', [setup(new_database)]):-
    default_user_id(UserId),
    ds_get(UserId, [password], Before),
    update_user(UserId, _{ type: admin, password: abc_not_used_before }, Update1),
    assertion(Update1.status = "success"),
    ds_get(UserId, [password], After),
    assertion(Before.password \= After.password), !.

test('Remove the user', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success"),
    remove_user(User.data, Removal),
    assertion(Removal.status = "success").

test('Remove the user, no authentication', [setup(new_database)]):-
    new_user(_{}, User),
    assertion(User.status = "success"),
    set_no_auth,
    remove_user(User.data, Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "Invalid or missing API key.").

test('Remove the user, last admin', [setup(new_database)]):-
    default_user_id(UserId),
    remove_user(UserId, Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "Cannot remove the last admin.").

test('Remove the user, has posts', [setup(new_database)]):-
    new_user(_{ username: 'author@example.com' }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    new_post(User.data, test_post, Post),
    assertion(Post.status = "success"),
    set_default_username('admin@example.com'),
    remove_user(User.data, Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "The user has posts.").

test('Remove non-existing user', [setup(new_database)]):-
    remove_user('xxx-user-not-exists', Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "The user does not exist.").

test('Remove the user, no admin right', [setup(new_database)]):-
    new_user(_{ username: 'author@example.com', type: author }, User),
    assertion(User.status = "success"),
    default_user_id(RemoveId),
    set_default_username('author@example.com'),
    remove_user(RemoveId, Removal),
    assertion(Removal.status = "error"),
    assertion(Removal.message = "The operation requires admin privileges.").

test('List of users', [setup(new_database)]):-
    list_users(List),
    assertion(List.status = "success"),
    assertion(is_list(List.data)),
    List.data = [User],
    assertion(get_dict(username, User, _)),
    assertion(get_dict(fullname, User, _)),
    assertion(get_dict(type, User, _)).

test('List of users, no admin right', [setup(new_database)]):-
    new_user(_{ username: 'author@example.com', type: author }, User),
    assertion(User.status = "success"),
    set_default_username('author@example.com'),
    list_users(List),
    assertion(List.status = "error"),
    assertion(List.message = "The operation requires admin privileges.").

test('List of users, no authentication', [setup(new_database)]):-
    set_no_auth,
    list_users(List),
    assertion(List.status = "error"),
    assertion(List.message = "Invalid or missing API key.").

:- end_tests(api_user).
