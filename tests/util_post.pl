% FIXME rename to util_entry.

:- module(util_post, [
    new_post/3,      % +AuthorId, +Slug, -Response
    new_post/4,      % +AuthorId, +Slug, +Override, -Response
    update_post/3,   % +PostId, +Override, -Response
    remove_post/2,   % +PostId, -Response
    list_posts/1,    % -Response,
    get_entry/2,     % +EntryId, -Response
    get_entry_info/2 % +EntryId, -Response
]).

:- use_module(util).

% FIXME merge into new_post/3.

new_post(AuthorId, Slug, Response):-
    new_post(AuthorId, Slug, _{}, Response).

new_post(AuthorId, Slug, Override, Response):-
    Dict = _{
        author: AuthorId,
        title: "Test post",
        slug: Slug,
        tags: [test, post],
        date_published: 1396216490,
        date_updated: 1396216490,
        commenting: true,
        published: true,
        content: "**test**",
        content_type: markdown,
        description: "Test",
        type: post,
        language: en
    },
    put_dict(Override, Dict, Data),
    request_post('/api/entry', Data, Response).

% Updates the given post.

update_post(PostId, Override, Response):-
    default_user_id(AuthorId),
    Dict = _{
        author: AuthorId,
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
        type: post,
        language: en
    },
    atom_concat('/api/entry/', PostId, Url),
    put_dict(Override, Dict, Data),
    request_put(Url, Data, Response).

% Removes the given post.

remove_post(EntryId, Response):-
    atom_concat('/api/entry/', EntryId, Path),
    request_del(Path, Response).

% Retrieves the list of posts.

list_posts(Response):-
    request_get('/api/entries/post', Response).

% Retrieves the given entry.

get_entry(EntryId, Response):-
    atom_concat('/api/entry/', EntryId, Path),
    request_get(Path, Response).

% Retrieves the given entry info.

get_entry_info(EntryId, Response):-
    atomic_list_concat(['/api/entry/', EntryId, '/info'], Path),
    request_get(Path, Response).
