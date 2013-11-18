:- module(blog, [
    setup/1,
    setup/2,
    enable_debug
]).

:- asserta(user:file_search_path(library, '/home/raivo/docstore/prolog')).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(error)).

:- use_module(library(docstore)).

:- use_module(blog_config).
:- use_module(blog_route).
:- use_module(blog_admin).
:- use_module(blog_atom).
:- use_module(blog_html).
:- use_module(blog_json).

enable_debug:-
    debug(blog_route).

setup(File):-
    docstore:open(File),
    init_config,
    init_users,
    init_meta,
    start_server.

setup(Mod, File):-
    docstore:open(File),
    init_config,
    init_users,
    init_meta,
    load_all(Mod),
    start_server.
    
init_config:-
    all(config, []), !,
    config_set(title, 'Untitled blog'),
    config_set(author, 'Name not set'),
    config_set(server_port, 8001),
    config_set(num_workers, 10),
    config_set(timeout_worker, 20),
    config_set(site, 'http://localhost:8001'),
    config_set(date_format, '%F'),
    config_set(meta_headers, []).
    
init_config.

init_users:-
    all(users, []), !,
    insert(users, [
        username('admin'),
        password('admin'),
        email('test@example.com'),
        name('Blog'),
        surname('Admin'),
        role(admin)
    ]).

init_users.

init_meta:-
    all(types, []), !,
    insert(types, [
        name(users),
        title(username),
        list([ email, username, role ]),
        detail([ email, username, role, name, surname ]),
        edit([ email, username, role, name, surname ]),
        props([
            username([
                type(line)
            ]),
            email([
                type(line)
            ]),
            name([
                type(line)
            ]),
            password([
                type(password)
            ]),
            surname([
                type(line)
            ]),
            role([
                type(choice),
                values([ normal, admin ])
            ])
        ])        
    ]),
    insert(types, [
        name(posts),
        title(title),
        order([
            property(date),
            direction(desc)
        ]),
        list([ title, published, commenting ]),
        detail([ title, slug, published, commenting, content ]),
        edit([ title, slug, published, commenting, content ]),
        props([
            date([
                type(datetime)
            ]),
            published([
                type(boolean)
            ]),
            commenting([
                type(boolean)
            ]),
            title([
                type(line)
            ]),
            slug([
                type(line)
            ]),
            content([
                type(multiline)
            ]),
            type([
                type(line)
            ])
        ])
    ]),
    insert(types, [
        name(config),
        title(name),
        list([ name, value ]),
        detail([ name, value ]),
        edit([ name, value ]),
        props([
            name([
                type(line)
            ]),
            value([
                type(line)
            ])
        ])        
    ]),
    insert(types, [
        name(comments),
        title(author),
        order([
            property(date),
            direction(desc)
        ]),
        list([ date, author ]),
        detail([ date, author, content ]),
        edit([ date, author, content ]),
        props([
            author([
                type(line)
            ]),
            content([
                type(multiline)
            ]),
            date([
                type(datetime)
            ])
        ])
    ]).

init_meta.
    
load_all(Mod):-
    module_property(Mod, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/views', ViewsDir),
    atom_concat(Dir, '/routes', RoutesDir),
    load_dir(ViewsDir),
    load_dir(RoutesDir).    
    
load_dir(Dir):-
    atom_concat(Dir, '/*.pl', Path),
    expand_file_name(Path, List),
    load_files(List, [must_be_module(true)]).

start_server:-
    config_get(server_port, Port),
    config_get(num_workers, Workers),
    config_get(timeout_worker, Timeout),
    http_server(blog_route, [
        port(Port),
        workers(Workers),
        timeout(Timeout)
    ]).