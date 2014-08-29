:- module(util, [
    new_database/0,
    set_default_username/1, % +Username
    default_user_id/1,      % -Id
    request_get/2,          % +Path, -Dict
    request_put/3,          % +Path, +DictIn, -DictOut
    request_del/2,          % +Path, -Dict
    request_post/3,         % +Path, +DictIn, -DictOut
    request_get_content/2,  % +Path, -String
    is_invalid_data/1       % +Response
]).

/** <module> Test utilities

The module contains utility predicates
for unit/integration testing.
*/

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(docstore)).

:- use_module(prolog/bc/bc_data).
:- use_module(prolog/bc/bc_data_user).
:- use_module(prolog/bc/bc_data_comment).
:- use_module(prolog/bc/bc_data_entry).
:- use_module(prolog/bc/bc_data_cur_user).

:- dynamic(default_username/1).
:- dynamic(no_auth/0).

% Recreates the test database.
% This also runs the initial migrations.

new_database:-
    bc_data_close,
    (   exists_file('test.docstore')
    ->  delete_file('test.docstore')
    ;   true),
    bc_data_open('test.docstore'),
    retractall(default_username(_)),
    asserta(default_username('admin@example.com')),
    retractall(no_auth).

% Sets default username.
% Call in the middle of test to
% set the user.

set_default_username(Username):-
    retractall(default_username(_)),
    asserta(default_username(Username)).

% Retrieves the default test user id.

default_user_id(UserId):-
    default_username(Username),
    ds_find(user, username=Username, [User]),
    User.'$id' = UserId.

% Auth key for the test user.

test_auth_key(Key):-
    default_username(Username),
    ds_find(user, username=Username, [key], [User]),
    User.key = Key.

request_get(Path, Dict):-
    test_auth_key(Key),
    Options = [ request_header('X-Key'=Key) ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Dict),
    close(Stream).

request_post(Path, In, Out):-
    test_auth_key(Key),
    Options = [ request_header('X-Key'=Key), post(json(In)) ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Out),
    close(Stream).

request_put(Path, In, Out):-
    test_auth_key(Key),
    Options = [ request_header('X-Key'=Key), post(json(In)), method(put) ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Out),
    close(Stream).

request_del(Path, Dict):-
    test_auth_key(Key),
    Options = [ request_header('X-Key'=Key), method(delete) ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Dict),
    close(Stream).

request_get_content(Path, String):-
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, []),
    read_string(Stream, _, String),
    close(Stream).

% FIXME rename to is_response_invalid_data

is_invalid_data(Response):-
    Response.status = "error",
    sub_string(Response.message, 0, _, _, "Invalid input").
