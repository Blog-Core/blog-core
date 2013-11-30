:- module(bc_data, [
    bc_register_collection/2, % +Name, +Type
    bc_collection/2           % ?Name, ?Type
]).

:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).
:- use_module(library(error)).

:- use_module(bc_doc).
:- use_module(bc_config).

%% bc_register_collection(+Name, +Type) is det.
%
% FIXME is_doc check.
% Registers a new collection for automatic
% admin interface. Docstore does not have
% to be open yet.
%
% Throws error(collection_exists(Name)) when
% the collection already exists.

bc_register_collection(Name, Type):-
    must_be(atom, Name),
    must_be(ground, Type),
    (   collection(Name, _)
    ->  throw(error(collection_exists(Name)))
    ;   assertz(collection(Name, Type))).

%% bc_collection(?Name, ?Type) is nondet.
%
% Queries currently registered collection
% types. Docstore does not have to be open yet.
    
bc_collection(Name, Type):-
    collection(Name, Type).

%% collection(?Name, ?Type) is nondet.
%
% Keeps metadata about document
% collections.

:- dynamic(collection/2).

% Hook for setting up basic configuration values.

:- ds_hook(open, init_config).

init_config:-
    ds_all(config, []),
    config_set(title, 'Untitled blog'),
    config_set(author, 'Name not set'),
    config_set(site, 'http://localhost:8001'),
    config_set(date_format, '%F'),
    config_set(excerpt_size, 100).

% Hook for creating the initial admin user.
    
:- ds_hook(open, init_users).

init_users:-
    ds_all(users, []),
    ds_uuid(Key),
    ds_insert(users, [
        username('admin'),
        password('admin'),
        email('test@example.com'),
        name('Blog'),
        surname('Admin'),
        role(admin),
        key(Key)
    ]).

% Configuration options.
    
:- bc_register_collection(config, [
    description('Configuration options.'),
    title(name),
    list([name, value]),
    detail([name, value]),
    edit([name, value]),
    props([
        name([type(line)]),
        value([type(line)])
    ])
]).

% Static HTML blocks.

:- bc_register_collection(blocks, [
    description('Static HTML blocks.'),
    title(name),
    order([property(date), direction(asc)]),
    list([name, slug]),
    detail([name, slug, content]),
    edit([name, slug, content]),
    props([
        name([type(line)]),
        slug([type(line)]),
        content([type(multiline)])
    ])
]).

% Blog posts/pages.

:- bc_register_collection(posts, [
    description('Blog posts. Can serve as pages/fragments too.'),
    title(title),
    order([property(date),direction(desc)]),
    list([title, published, commenting]),
    detail([title, slug, description, published,
        commenting, content, tags, date]),
    edit([title, slug, description, published,
        commenting, content, tags, date]),
    props([
        date([type(datetime)]),
        published([type(boolean)]),
        commenting([type(boolean)]),
        title([type(line)]),
        slug([type(line)]),
        description([type(multiline)]),
        content([type(multiline)]),
        type([type(line)]),
        tags([type(tags)])
    ])
]).

% Post comments.

:- bc_register_collection(comments, [
    description('Blog post comments.'),
    title(name),
    order([property(date), direction(desc)]),
    list([date, name, post]),
    detail([date, name, content, post]),
    edit([date, name, content]),
    props([
        name([type(line)]),
        content([type(multiline)]),
        date([type(datetime)]),
        post([type(ref)])
    ])
]).

% Blog users/post authors.

:- bc_register_collection(users, [
    description('Blog users'),
    title(username),
    list([email, username, role]),
    detail([email, username, role,
        name, surname, key]),
    edit([email, username, role,
        name, surname, key, password]),
    props([
        username([type(line)]),
        email([type(line)]),
        name([type(line)]),
        password([type(line)]),
        surname([type(line)]),
        role([type(choice),values([normal, admin])]),
        key([type(line)])
    ])
]).

% Hook to turn post content into HTML.

:- ds_hook(posts, before_save, convert_content).

convert_content(In, Out):-
    memberchk(content(Content), In),
    atom_codes(Content, Codes),
    (   md_html(Codes, Html)
    ->  Out = [html(Html)|In]
    ;   doc_id(In, Id),
        throw(error(cannot_convert_post(Id)))).
        
% Hook to remove comments when a post is removed.

:- ds_hook(posts, before_remove, remove_comments).

remove_comments(Id):-
    ds_remove(comments, post=Id).

% Hook to add API key to the user.

:- ds_hook(users, before_save, add_api_key).

% Key cleared, generate new.

add_api_key(In, Out):-
    memberchk(key(''), In),
    ds_uuid(New),
    doc_replace(key, New, In, Out).
