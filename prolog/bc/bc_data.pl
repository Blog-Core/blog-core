:- module(bc_data, [
    bc_data_open/1,           % +File
    bc_data_close/0,
    bc_register_collection/2, % +Name, +Type
    bc_unregister_all/0,
    bc_collection/2           % ?Name, ?Type
]).

:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).
:- use_module(library(error)).

:- use_module(bc_config).

% Threadlocal for the current author.
% Automatically set/unset by the admin interface.

:- thread_local(author/1).

%! bc_register_collection(+Name, +Type) is det.
%
% Registers a new collection for automatic
% admin interface. Docstore does not have
% to be open yet.
% When a collection with same name has
% already been registered it is replaced.

bc_register_collection(Name, Type):-
    must_be(atom, Name),
    must_be(dict, Type),
    retractall(bc_collection(Name, _)),
    assertz(bc_collection(Name, Type)).

bc_unregister_all:-
    retractall(bc_collection(_, _)).

%! bc_collection(?Name, ?Type) is nondet.
%
% Queries currently registered collection
% types. Docstore does not have to be open yet.

:- dynamic(bc_collection/2).

bc_data_open(File):-
    ds_open(File),
    (   ds_all(config, [])
    ->  init_config,
        init_users
    ;   true).

bc_data_close:-
    ds_close,
    bc_unregister_all.

init_config:-
    config_set(title, 'Untitled blog'),
    config_set(author, 'Name not set'),
    config_set(site, 'http://localhost:8001'),
    config_set(date_format, '%F'),
    config_set(excerpt_size, 100).

init_users:-
    ds_uuid(Key),
    ds_insert(user{
        username: 'admin',
        password: 'admin',
        email: 'test@example.com',
        name: 'Blog',
        surname: 'Admin',
        role: admin,
        key: Key
    }).
    
:- bc_register_collection(config, config{
    description: 'Configuration options.',
    title: name,
    list: [name, value],
    detail: [name, value],
    edit: [name, value],
    properties: _{
        name: _{ type: line },
        value: _{ type: line }
    }
}).

% Static HTML blocks.

:- bc_register_collection(block, block{
    description: 'Static HTML blocks.',
    title: name,
    order: _{ property: date, direction: desc },
    list: [name, slug],
    detail: [name, slug, content],
    edit: [name, slug, content],
    properties: _{
        name: _{ type: line },
        slug: _{ type: line },
        content: _{ type: multiline }
    }
}).

% Blog posts/pages.

:- bc_register_collection(post, post{
    description: 'Blog posts and pages.',
    title: title,
    order: _{ property: date, direction: desc },
    list: [title, published, commenting],
    detail: [title, slug, description, published,
        commenting, content, tags, date, author],
    edit: [title, slug, description, published,
        commenting, content, tags, date, author],
    properties: _{
        date: _{ type: datetime },
        published: _{ type: boolean },
        commenting: _{ type: boolean },
        title: _{ type: line },
        slug: _{ type: line },
        description: _{ type: multiline },
        content: _{ type: multiline },
        type: _{ type: line },
        tags: _{ type: tags },
        author: _{ type: author }
    }
}).

% Post comments.

:- bc_register_collection(comment, comment{
    description: 'Blog post comments.',
    title: name,
    order: _{ property: date, direction: desc },
    list: [date, name, post],
    detail: [date, name, content, post],
    edit: [date, name, content],
    properties: _{
        name: _{ type: line },
        content: _{ type: multiline },
        date: _{ type: datetime },
        post: _{ type: ref }
    }
}).

% Blog users/post authors.

:- bc_register_collection(user, user{
    description: 'Users',
    title: username,
    list: [email, username, role],
    detail: [email, username, role,
        name, surname, key],
    edit: [email, username, role,
        name, surname, key, password],
    properties: _{
        username: _{ type: line },
        email: _{ type: line },
        name: _{ type: line },
        password: _{ type: line },
        surname: _{ type: line },
        role: _{ type: choice, values: [normal, admin] },
        key: _{ type: line }
    }
}).

% Hook to turn post content into HTML.

:- ds_hook(post, before_save, convert_content).

convert_content(In, Out):-
    (   get_dict(content, In, Content)
    ->  md_html_string(Content, Html),
        put_dict(html, In, Html, Out)
    ;   Out = In).

:- ds_hook(post, before_save, set_author).

set_author(In, Out):-
    (   get_dict(author, In, "")
    ->  author(User),
        get_dict_ex('$id', User, Id),
        atom_string(Id, IdString),
        put_dict(author, In, IdString, Out)
    ;   Out = In).

% Hook to remove comments when a post is removed.

:- ds_hook(post, before_remove, remove_comments).

remove_comments(Id):-
    ds_remove(comment, post=Id).

% Hook to add API key to the user.

:- ds_hook(user, before_save, add_api_key).

% Key cleared, generate new.

add_api_key(In, Out):-
    (   get_dict(key, In, "")
    ->  ds_uuid(Key),
        put_dict(key, In, Key, Out)
    ;   Out = In).
