:- module(util, [
    new_database/0,
    request_get/2,        % +Path, -Dict
    request_put/3,        % +Path, +DictIn, -DictOut
    request_del/2,        % +Path, -Dict
    request_post/3,       % +Path, +DictIn, -DictOut
    request_get_content/2 % +Path, -String
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

% Recreates the test database.

new_database:-
    bc_data_close,
    (   exists_file('test.docstore')
    ->  delete_file('test.docstore')
    ;   true),
    bc_data_open('test.docstore'),
    bc_user_save(user{
        username: default_test,
        password: default_test,
        fullname: 'Default Test User',
        type: admin
    }, UserId),
    ds_get(UserId, User),
    bc_set_user(User),
    bc_entry_save(entry{
        author: UserId,
        title: "Default Test post",
        slug: 'default-test-post',
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**test**",
        content_type: markdown,
        description: "Test",
        type: post
    }, PostId),
    bc_unset_user,
    bc_comment_save(PostId, comment{
        author: "RLa",
        content: "Test comment",
        question: 1,
        answer: '3'
    }).

% Auth key for the test user.

test_auth_key(Key):-
    ds_find(user, username=default_test, [key], [User]),
    get_dict_ex(key, User, Key).

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
