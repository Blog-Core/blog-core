:- module(bc_data, [
    bc_data_init/1,
    reinit_types
]).

:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).

:- use_module(bc_doc).
:- use_module(bc_config).

bc_data_init(File):-
    ds_open(File),
    init_config,
    init_users,
    init_meta.

% Sets up basic configuration values.

init_config:-
    (   ds_all(config, [])
    ->  config_set(title, 'Untitled blog'),
        config_set(author, 'Name not set'),
        config_set(server_port, 8001),
        config_set(num_workers, 10),
        config_set(timeout_worker, 20),
        config_set(site, 'http://localhost:8001'),
        config_set(date_format, '%F'),
        config_set(excerpt_size, 100)
    ;   true).
    
% Creates the initial admin user.

init_users:-
    (   ds_all(users, [])
    ->  ds_uuid(Key),
        ds_insert(users, [
            username('admin'),
            password('admin'),
            email('test@example.com'),
            name('Blog'),
            surname('Admin'),
            role(admin),
            key(Key)
        ])
    ;   true).

% Created entries for the admin interface.
    
init_meta:-
    (   ds_all(types, [])
    ->  ds_insert(types, [
            name(users),
            description('Blog users'),
            title(username),
            list([ email, username, role ]),
            detail([ email, username, role, name, surname, key ]),
            edit([ email, username, role, name, surname, key, password ]),
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
                    type(line)
                ]),
                surname([
                    type(line)
                ]),
                role([
                    type(choice),
                    values([ normal, admin ])
                ]),
                key([
                    type(line)                    
                ])
            ])        
        ]),
        ds_insert(types, [
            name(posts),
            description('Blog posts. Can serve as pages/fragments too.'),
            title(title),
            order([
                property(date),
                direction(desc)
            ]),
            list([ title, published, commenting ]),
            detail([ title, slug, published, commenting, content, tags, date ]),
            edit([ title, slug, published, commenting, content, tags, date ]),
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
                ]),
                tags([
                    type(tags)
                ])
            ])
        ]),
        ds_insert(types, [
            name(config),
            description('Configuration options.'),
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
            description('Blog post comments.'),
            title(name),
            order([
                property(date),
                direction(desc)
            ]),
            list([ date, name, post ]),
            detail([ date, name, content, post ]),
            edit([ date, name, content ]),
            props([
                name([
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
        ]),
        ds_insert(types, [
            name(blocks),
            description('Static HTML blocks.'),
            title(name),
            order([
                property(date),
                direction(asc)
            ]),
            list([name, slug]),
            detail([name, slug, content]),
            edit([name, slug, content]),
            props([
                name([
                    type(line)
                ]),
                slug([
                    type(line)
                ]),
                content([
                    type(multiline)
                ])
            ])
        ]),
        ds_insert(types, [
            name(meta_headers),
            description('Custom meta headers.'),
            title(name),
            order([
                property(name),
                direction(asc)
            ]),
            list([name, value]),
            detail([name, value]),
            edit([name, value]),
            props([
                name([
                    type(line)
                ]),
                value([
                    type(line)
                ])
            ])
        ])
    ; true).
    
% Reloads all collection types.
% Helper for development.
    
reinit_types:-
    ds_remove_col(types),
    init_meta.

% Hook to turn post content into HTML.

:- ds_hook(posts, before_save, convert_content).

convert_content(In, Out):-
    memberchk(content(Content), In),
    atom_codes(Content, Codes),
    (   md_html(Codes, Html)
    ->  Out = [html(Html)|In]
    ;   doc_id(In, Id),
        throw(error(cannot_convert_post(Id)))).

% Hook to add API key to the user.

:- ds_hook(users, before_save, add_api_key).

% Key cleared, generate new.

add_api_key(In, Out):-
    memberchk(key(''), In),
    ds_uuid(New),
    doc_replace(key, New, In, Out).

% Hook to remove comments when a post is removed.

:- ds_hook(posts, before_remove, remove_comments).
    
remove_comments(Id):-
    ds_remove(comments, post=Id).
