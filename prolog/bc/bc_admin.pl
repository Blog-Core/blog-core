:- module(bc_admin, []).

:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).

:- use_module(library(docstore)).
:- use_module(library(ar_router)).

:- use_module(bc_doc).
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
% Requires role admin.

auth(Next):-
    (   auth_x_key_admin
    ->  call(Next)
    ;   reply_error(101)).

auth_x_key_admin:-
    http_current_request(Request),
    memberchk(x_key(Key), Request),
    ds_find(users, (key=Key, role=admin), [role], [_]).
    
% Gives all documents in the collection.
    
:- route_get(api/col/Col, [auth], doc_all(Col)).

doc_all(Col):-
    ds_all(Col, Docs),
    maplist(doc_to_json, Docs, Json),
    reply_success(Json).
    
% Gives all registered types.
    
:- route_get(api/types, [auth], types_all).

types_all:-
    findall([name(Name)|Type], bc_collection(Name, Type), Types),
    maplist(doc_to_json, Types, Json),
    reply_success(Json).

% Stores new document in the collection.
% Replies back id.

:- route_post(api/col/Col/doc, [auth], doc_insert(Col)).

doc_insert(Col):-
    http_current_request(Req),
    http_read_data(Req, Json, []),
    json_to_doc(Json, Doc),
    ds_insert(Col, Doc, Id),
    reply_success(Id).
    
% Gives document type by collection name.

:- route_get(api/col/Col/type, [auth], col_type(Col)).

col_type(Col):-
    (   bc_collection(Col, Type)
    ->  doc_to_json([name(Col)|Type], Json),
        reply_success(Json)
    ;   reply_error(103)).

% Gives single document by id.

:- route_get(api/doc/Id, [auth], doc_get(Id)).

doc_get(Id):-
    (   ds_get(Id, Doc)
    ->  doc_to_json(Doc, Json),
        reply_success(Json)
    ;   reply_error(104)).
    
% Gives document type by document id.
    
:- route_get(api/doc/Id/type, [auth], doc_type(Id)).
    
doc_type(Id):-
    (   ds_collection(Id, Col),
        bc_collection(Col, Type)
    ->  doc_to_json([name(Col)|Type], Json),
        reply_success(Json)
    ;   reply_error(105)).
    
% Updates document by id.

:- route_put(api/doc/Id, [auth], doc_update(Id)).

doc_update(Id):-
    http_current_request(Req),
    http_read_data(Req, Json, []),
    json_to_doc(Json, Doc),
    (   memberchk('$id'(Id), Doc)
    ->  ds_update(Doc)
    ;   ds_update(['$id'(Id)|Doc])),
    reply_success(Id).

% Removes the given document.
% Replies back id.
    
:- route_del(api/doc/Id, [auth], doc_remove(Id)).
    
doc_remove(Id):-
    ds_remove(Id),
    reply_success(Id).

% Logins into the system with username/password.
% When logic succeeds, sends user key back.
% Otherwise sends error 102.
    
:- route_post(api/login, login).

login:-
    http_current_request(Req),
    http_read_data(Req, json(Data), []),
    memberchk(username=User, Data),
    memberchk(password=Pass, Data),
    (   Cond = (username=User, password=Pass),
        ds_find(users, Cond, [Doc])
    ->  doc_get(key, Doc, Key),
        reply_success(Key)
    ;   reply_error(102)).

% Sends JSON response with Data
% and success.
    
reply_success(Data):-
    reply_json(json([status=success, data=Data])).

% Sends error JSON response with Code.
    
reply_error(Code):-
    reply_json(json([status=error, code=Code])).
