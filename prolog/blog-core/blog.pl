:- module(blog, [
    setup/1,
    setup/2,
    enable_debug
]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(error)).

:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).

:- use_module(blog_config).
:- use_module(blog_admin).
:- use_module(blog_atom).
:- use_module(blog_doc).

enable_debug:-
    debug(blog_route).

setup(File):-
    ds_open(File),
    init_config,
    init_users,
    init_meta,
    start_server.

setup(Mod, File):-
    ds_open(File),
    init_config,
    init_users,
    init_meta,
    load_all(Mod),
    start_server.
    
init_config:-
    ds_all(config, []), !,
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
    ds_all(users, []), !,
    ds_insert(users, [
        username('admin'),
        password('admin'),
        email('test@example.com'),
        name('Blog'),
        surname('Admin'),
        role(admin)
    ]).

init_users.

init_meta:-
    ds_all(types, []), !,
    ds_insert(types, [
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
    ds_insert(types, [
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
    ds_insert(types, [
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
    ds_insert(types, [
        name(comments),
        title(author),
        order([
            property(date),
            direction(desc)
        ]),
        list([ date, author, post ]),
        detail([ date, author, content, post ]),
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
            ]),
            post([
                type(ref)
            ])
        ])
    ]).

init_meta.

% Hook to turn post content into HTML.

:- ds_hook(posts, before_save, convert_content).

convert_content(In, Out):-
    prop_get(content, In, Content),
    atom_codes(Content, Codes),
    (md_html(Codes, Html)
    ->  true
    ;   prop_get('$id', In, Id),
        throw(error(cannot_convert_post(Id)))),
    Out = [ html(Html)|In ].
    
% Hook to remove comments when a post is removed.

:- ds_hook(posts, before_remove, remove_comments).

remove_comments(Id):-
    ds_remove(comments, post = Id).
    
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