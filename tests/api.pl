:- begin_tests(api).

:- use_module(util).

% Add a new user.

test('POST /api/user', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author }, Dict),
    get_dict_ex(status, Dict, "success").

% Update an user.

test('PUT /api/user/Id', [setup(new_database)]):-
    request_post('/api/user', _{
        username: testa,
        password: test123,
        fullname: 'Test A',
        type: author }, Dict1),
    get_dict_ex(status, Dict1, "success"),
    get_dict_ex(data, Dict1, Id),
    atom_concat('/api/user/', Id, Url),
    request_put(Url, _{
        username: testb,
        password: test321,
        fullname: 'Test B',
        type: author }, Dict2),
    get_dict_ex(status, Dict2, "success").

% Remove an user.

test('DELETE /api/user/Id', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author }, Dict1),
    get_dict_ex(status, Dict1, "success"),
    get_dict_ex(data, Dict1, Id),
    atom_concat('/api/user/', Id, Url),
    request_del(Url, Dict2),
    get_dict_ex(status, Dict2, "success").

% Authenticate user.

test('POST /api/auth', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author }, User),
    get_dict_ex(status, User, "success"),
    request_post('/api/auth', _{
        username: test,
        password: test123
    }, Auth),
    get_dict_ex(status, Auth, "success"),
    get_dict_ex(data, Auth, _).

% Invalid credentials.

test('POST /api/auth (invalid credentials)', [setup(new_database)]):-
    request_post('/api/auth', _{
        username: not_exists,
        password: test123
    }, Auth),
    get_dict_ex(status, Auth, "error"),
    get_dict_ex(message, Auth, "Invalid auth credentials.").

test('POST /api/auth (invalid data)', [setup(new_database)]):-
    request_post('/api/auth', _{
        password: test123
    }, Auth),
    get_dict_ex(status, Auth, "error"),
    get_dict_ex(message, Auth, "Invalid input: [no_key(#,username)].").

test('POST /api/post', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author }, User),
    get_dict_ex(status, User, "success"),
    get_dict_ex(data, User, Author),
    request_post('/api/post', _{
        author: Author,
        title: "Test post",
        slug: "test-post",
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**test**",
        content_type: markdown,
        description: "Test",
        type: post
    }, Post),
    get_dict_ex(status, Post, "success"),
    get_dict_ex(data, Post, _).

test('PUT /api/post', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author }, User),
    get_dict_ex(status, User, "success"),
    get_dict_ex(data, User, Author),
    request_post('/api/post', _{
        author: Author,
        title: "Test post",
        slug: "test-post",
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**test**",
        content_type: markdown,
        description: "Test",
        type: post
    }, Post),
    get_dict_ex(status, Post, "success"),
    get_dict_ex(data, Post, PostId),
    atom_concat('/api/post/', PostId, UpdateUrl),
    request_put(UpdateUrl, _{
        author: Author,
        title: "Test post 1",
        slug: "test-post-1",
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**test 1**",
        content_type: markdown,
        description: "Test 1",
        type: post
    }, Update),
    get_dict_ex(status, Update, "success").

:- end_tests(api).
