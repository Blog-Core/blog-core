:- module(util_user, [
    new_user/2,             % +Override, -Response
    new_user_no_password/1, % -Response
    get_user/2,             % +UserId, -Response
    update_user/3,          % +UserId, +Override, -Response
    remove_user/2,          % +UserId, -Response
    list_users/1,           % -Response
    authenticate_user/3,    % +Username, +Password, -Response
    authenticate_invalid/1  % -Response
]).

:- use_module(util).

% Creates new user with overridable
% data. Gives back API response.

new_user(Override, Response):-
    Dict = _{
        username: 'test@example.com',
        password: test123,
        fullname: 'Test',
        type: author,
        link: ""
    },
    put_dict(Override, Dict, Data),
    request_post('/api/user', Data, Response).

% Adds new user, does not set password.

new_user_no_password(Response):-
    Dict = _{
        username: 'test@example.com',
        fullname: 'Test',
        type: author,
        link: ""
    },
    request_post('/api/user', Dict, Response).

% Retrieves the given user.

get_user(UserId, Response):-
    atomic_list_concat(['/api/user/', UserId], Path),
    request_get(Path, Response).

% Updates the given user.

update_user(UserId, Override, Response):-
    Dict = _{
        username: 'test@example.com',
        fullname: 'Test',
        type: author,
        link: ""
    },
    atom_concat('/api/user/', UserId, Url),
    put_dict(Override, Dict, Data),
    request_put(Url, Data, Response).

% Removed the given user.

remove_user(UserId, Response):-
    atom_concat('/api/user/', UserId, Url),
    request_del(Url, Response).

% List of users.

list_users(Response):-
    request_get('/api/users', Response).

% Authenticates with the given
% credentials.

authenticate_user(Username, Password, Response):-
    request_post('/api/auth',
        _{ username: Username, password: Password }, Response).

% Authenticates with invalid data.

authenticate_invalid(Response):-
    request_post('/api/auth',
        _{}, Response).
