:- begin_tests(api).

:- use_module(library(docstore)).
:- use_module(util).

% Update an user.

test('PUT /api/user/Id', [setup(new_database)]):-
    request_post('/api/user', _{
        username: testa,
        password: test123,
        fullname: 'Test A',
        type: author,
        files: true,
        link: "" }, Dict1),
    Dict1.status = "success",
    Dict1.data = Id,
    atom_concat('/api/user/', Id, Url),
    request_put(Url, _{
        username: testb,
        password: test321,
        fullname: 'Test B',
        type: author,
        files: true,
        link: "" }, Dict2),
    Dict2.status = "success".

% Remove an user.

test('DELETE /api/user/Id', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author,
        files: true,
        link: "" }, Dict1),
    Dict1.status = "success",
    Dict1.data = Id,
    atom_concat('/api/user/', Id, Url),
    request_del(Url, Dict2),
    Dict2.status = "success".

% Authenticate user.

test('POST /api/auth', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author,
        files: true,
        link: "" }, User),
    User.status = "success",
    request_post('/api/auth', _{
        username: test,
        password: test123
    }, Auth),
    Auth.status = "success",
    Auth.data = _.

% Invalid credentials.

test('POST /api/auth (invalid credentials)', [setup(new_database)]):-
    request_post('/api/auth', _{
        username: not_exists,
        password: test123
    }, Auth),
    Auth.status = "error",
    Auth.message = "Invalid auth credentials.".

test('POST /api/auth (invalid data)', [setup(new_database)]):-
    request_post('/api/auth', _{
        password: test123
    }, Auth),
    Auth.status = "error",
    Auth.message = "Invalid input: [no_key(#,username)].".

test('GET /api/configs', [setup(new_database)]):-
    request_get('/api/configs', Dict),
    Dict.status = "success",
    Dict.data = List,
    assertion(is_list(List)),
    List = [TitleConfig],
    TitleConfig.name = _,
    TitleConfig.value = _.

test('PUT /api/config', [setup(new_database)]):-
    request_put('/api/config', _{
        name: "title",
        value: "Updated title"
    }, Dict),
    Dict.status = "success".

:- end_tests(api).
