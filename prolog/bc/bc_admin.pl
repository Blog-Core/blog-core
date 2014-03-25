:- module(bc_admin, []).

:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(docstore)).
:- use_module(library(arouter)).

:- use_module(bc_data).

% Sends the main admin HTML file.

:- route_get(admin, reply_file('admin.html')).

% Sends the given file as response.

:- route_get(admin/File, reply_file(File)).

reply_file(File):-
    module_property(bc_admin, file(ModFile)),
    file_directory_name(ModFile, ModDir),
    atomic_list_concat([ModDir, '/public/', File], FullPath),
    \+ sub_atom(FullPath, _, _, _, '..'),
    http_current_request(Request),
    http_reply_file(FullPath, [unsafe(true)], Request).

% Helper to authenticate the current request.
% Requires role admin. Populates the thread-
% local bc_data:author.

:- meta_predicate(auth(0)).

auth(Next):-
    (   http_current_request(Request),
        memberchk(x_key(Key), Request),
        atom_string(Key, KeyString),
        ds_find(user, (key=KeyString, role="admin"), [role], [User])
    ->  setup_call_cleanup(assertz(bc_data:author(User)),
            call(Next), retractall(bc_data:author(_)))
    ;   reply_error(101)).

% Gives all documents in the collection.
% Retrieves subset of properties. Only
% those properties are retrieved that
% are in the list field of the type.
% FIXME order property values also.
% FIXME order property is doubled.
    
:- route_get(api/collection/Name,
    auth, collection(Name)).

collection(Name):-
    (   bc_collection(Name, Type)
    ->  get_dict_ex(list, Type, Properties),
        (   get_dict(order, Type, Order)
        ->  get_dict_ex(property, Order, Prop),
            AllProperties = [Prop|Properties]
        ;   AllProperties = Properties),
        ds_all(Name, AllProperties, Docs),
        reply_success(Docs)
    ;   reply_error(103)).

% Gives all registered types.
    
:- route_get(api/types, auth, types_all).

types_all:-
    findall(_{ name: Name, def: Type },
        bc_collection(Name, Type), Types),
    reply_success(Types).

% Stores new document in the collection.
% Replies back id.

:- route_post(api/collection/Collection,
    auth, new_document(Collection)).

new_document(Collection):-
    http_current_request(Request),
    http_read_json_dict(Request, Dict),
    ds_insert(Collection, Dict, Id),
    reply_success(Id).

% Gives document type by collection name.

:- route_get(api/collection/Collection/type,
    auth, collection_type(Collection)).

collection_type(Collection):-
    (   bc_collection(Collection, Type)
    ->  put_dict(name, Type, Collection, Out),
        reply_success(Out)
    ;   reply_error(103)).

% Gives single document by id.
% Retrieves all fields.

:- route_get(api/document/Id,
    auth, document(Id)).

document(Id):-
    (   ds_get(Id, Dict)
    ->  reply_success(Dict)
    ;   reply_error(104)).

% Gives document type by document id.
    
:- route_get(api/document/Id/type,
    auth, document_type(Id)).
    
document_type(Id):-
    (   ds_collection(Id, Collection),
        bc_collection(Collection, Type)
    ->  put_dict(name, Type, Collection, Out),
        reply_success(Out)
    ;   reply_error(105)).

% Updates document by id.

:- route_put(api/document/Id,
    auth, update_document(Id)).

update_document(Id):-
    http_current_request(Request),
    http_read_json_dict(Request, Dict),
    (   del_dict('$id', Dict, _, Tmp)
    ->  true
    ;   Tmp = Dict),
    put_dict('$id', Tmp, Id, Update),
    ds_update(Update),
    reply_success(Id).

% Removes the given document.
% Replies back id.
    
:- route_del(api/document/Id,
    auth, remove_document(Id)).
    
remove_document(Id):-
    ds_remove(Id),
    reply_success(Id).

% Logins into the system with username/password.
% When logic succeeds, sends user key back.
% Otherwise sends error 102.
    
:- route_post(api/login, login).

login:-
    http_current_request(Request),
    http_read_json_dict(Request, Dict),
    (   get_dict(username, Dict, User),
        get_dict(password, Dict, Pass)
    ->  (   Cond = (username=User, password=Pass),
            ds_find(user, Cond, [Doc])
        ->  get_dict(key, Doc, Key),
            reply_success(Key)
        ;   reply_error(102))
    ;   reply_error(106)).

% Sends JSON response with Data
% and success.
    
reply_success(Data):-
    reply_json(_{ status: success, data: Data }).

% Sends error JSON response with Code.
    
reply_error(Code):-
    reply_json(_{ status: error, code: Code }).
