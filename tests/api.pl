:- begin_tests(api).

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).

request_get(Path, Dict):-
    Options = [
        request_header('X-Key'='b4184b55-0af1-451a-aed7-c056263ee3d3')
    ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Dict),
    close(Stream).

request_post(Path, In, Out):-
    Options = [
        request_header('X-Key'='b4184b55-0af1-451a-aed7-c056263ee3d3'),
        post(json(In))
    ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Out),
    close(Stream).

request_put(Path, In, Out):-
    Options = [
        request_header('X-Key'='b4184b55-0af1-451a-aed7-c056263ee3d3'),
        post(json(In)),
        method(put)
    ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Out),
    close(Stream).

request_del(Path, Dict):-
    Options = [
        request_header('X-Key'='b4184b55-0af1-451a-aed7-c056263ee3d3'),
        method(delete)
    ],
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, Options),
    json_read_dict(Stream, Dict),
    close(Stream).

test('GET /api/collection/user'):-
    request_get('/api/collection/user', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, [User]),
    get_dict_ex(email, User, "test@example.com").

test('GET /api/types'):-
    request_get('/api/types', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, [Type|_]),
    get_dict_ex(name, Type, _),
    get_dict_ex(def, Type, Def),
    get_dict_ex(description, Def, _),
    get_dict_ex(detail, Def, _),
    get_dict_ex(edit, Def, _),
    get_dict_ex(list, Def, _).

test('POST /api/collection/user'):-
    request_post('/api/collection/config',
        _{ name: test, value: abc }, Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, _).

test('GET /api/collection/config/type'):-
    request_get('/api/collection/config/type', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, Data),
    get_dict_ex(description, Data, _),
    get_dict_ex(detail, Data, _),
    get_dict_ex(edit, Data, _),
    get_dict_ex(list, Data, _),
    get_dict_ex(name, Data, "config"),
    get_dict_ex(properties, Data, _).

test('GET /api/document/Id'):-
    request_get('/api/document/d2f1b828-8063-446c-a1b7-4a151911fd27', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, Data),
    get_dict_ex(name, Data, _),
    get_dict_ex(value, Data, _).

test('GET /api/document/Id/type'):-
    request_get('/api/document/d2f1b828-8063-446c-a1b7-4a151911fd27/type', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, Data),
    get_dict_ex(description, Data, _),
    get_dict_ex(detail, Data, _),
    get_dict_ex(edit, Data, _),
    get_dict_ex(list, Data, _),
    get_dict_ex(name, Data, "config"),
    get_dict_ex(properties, Data, _).

test('PUT /api/document/Id'):-
    request_put('/api/document/d2f1b828-8063-446c-a1b7-4a151911fd27',
        _{ value: abc }, Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, _).

test('DELETE /api/document/Id'):-
    request_del('/api/document/abc-not-exist', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, _).

:- end_tests(api).
