:- begin_tests(api).

:- use_module(library(docstore)).
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

test('POST /api/entry', [setup(new_database)]):-
    request_post('/api/user', _{
        username: test,
        password: test123,
        fullname: 'Test',
        type: author }, User),
    get_dict_ex(status, User, "success"),
    get_dict_ex(data, User, Author),
    request_post('/api/entry', _{
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
    request_post('/api/entry', _{
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
    atom_concat('/api/entry/', PostId, UpdateUrl),
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

test('GET /api/entries/post', [setup(new_database)]):-
    request_get('/api/entries/post', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, List),
    assertion(is_list(List)),
    List = [Post],
    get_dict_ex(title, Post, _),
    get_dict_ex(author, Post, _).

test('GET /api/entry/Id', [setup(new_database)]):-
    ds_all(entry, [Post]),
    get_dict_ex('$id', Post, Id),
    atom_concat('/api/entry/', Id, Path),
    request_get(Path, Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, PostDict),
    get_dict_ex(title, PostDict, _),
    get_dict_ex(author, PostDict, _),
    get_dict_ex(content, PostDict, _).

test('GET /api/configs', [setup(new_database)]):-
    request_get('/api/configs', Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, List),
    assertion(is_list(List)),
    List = [TitleConfig],
    get_dict_ex(name, TitleConfig, _),
    get_dict_ex(value, TitleConfig, _).

test('PUT /api/config', [setup(new_database)]):-
    request_put('/api/config', _{
        name: "title",
        value: "Updated title"
    }, Dict),
    get_dict_ex(status, Dict, "success").

test('POST /api/post/Id/comment', [setup(new_database)]):-
    ds_all(entry, [Post]),
    get_dict_ex('$id', Post, Id),
    atomic_list_concat(['/api/post/', Id, '/comment'], Path),
    request_post(Path, _{
        author: "RLa",
        content: "Test comment",
        question: 1,
        answer: "3"
    }, Dict),
    get_dict_ex(status, Dict, "success").

test('GET /api/post/Id/comments', [setup(new_database)]):-
    ds_all(entry, [Post]),
    get_dict_ex('$id', Post, Id),
    atomic_list_concat(['/api/post/', Id, '/comments'], Path),
    request_get(Path, Dict),
    get_dict_ex(status, Dict, "success"),
    get_dict_ex(data, Dict, List),
    assertion(is_list(List)),
    List = [Comment],
    get_dict_ex(author, Comment, _),
    get_dict_ex(date, Comment, _).

:- end_tests(api).
