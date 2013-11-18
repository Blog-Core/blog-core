:- module(blog_admin, []).

:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client), [ http_read_data/3 ]).
:- use_module(library(http/http_session)).
:- use_module(library(debug)).
:- use_module(library(docstore)).
:- use_module(blog_route).
:- use_module(blog_json).
:- use_module(blog_prop).

:- http_get(admin, reply_file('admin.html')).

:- http_get(admin/File, reply_file(File)).

% Sends the given file as response.

reply_file(File, _):-
    module_property(blog_admin, file(ModFile)),
    file_directory_name(ModFile, ModDir),
    atomic_list_concat([ ModDir, '/public/', File ], FullPath),
    send_file(FullPath).

% Helper to authenticate the current request.
% Requires role admin.

auth(Next, _):-
    http_session_data(user(User)),
    memberchk(role(admin), User), !,
    call(Next).
    
auth(_, _):-
    reply_json(json([ status = error, code = 101 ])).
    
% Gives all documents in the collection.
    
:- http_get(api/col/Col, [ auth ], doc_all(Col)).

doc_all(Col, _):-
    all(Col, Docs),
    maplist(doc_to_json, Docs, JSONDocs),
    reply_json(json([ status = success, data = JSONDocs ])).

% Stores new document in the collection.
% Replies back id.

:- http_post(api/col/Col/doc, [ auth ], doc_insert(Col)).

doc_insert(Col, Req):-
    http_read_data(Req, JSONDoc, []),
    json_to_doc(JSONDoc, Doc),
    insert(Col, Doc, Id),
    reply_json(json([ status = success, data = Id ])).
    
% Gives document type by collection name.

:- http_get(api/col/Col/type, [ auth ], col_type(Col)).

col_type(Col, _):-
    find(types, name = Col, [ Doc ]),
    doc_to_json(Doc, JSONDoc),
    reply_json(json([ status = success, data = JSONDoc ])).
    
% Gives single document by id.

:- http_get(api/doc/Id, [ auth ], doc_get(Id)).

doc_get(Id, _):-
    get(Id, Doc),
    doc_to_json(Doc, JSONDoc),
    reply_json(json([ status = success, data = JSONDoc ])).
    
% Gives document type by document id.
    
:- http_get(api/doc/Id/type, [ auth ], doc_type(Id)).
    
doc_type(Id, _):-
    collection(Id, Col),
    find(types, name = Col, [Doc]),
    doc_to_json(Doc, JSONDoc),
    reply_json(json([ status = success, data = JSONDoc ])).
    
% Updates document by id.

:- http_put(api/doc/Id, [ auth ], doc_update(Id)).

doc_update(Id, Req):-
    http_read_data(Req, JSONDoc, []),
    json_to_doc(JSONDoc, Doc),
    (memberchk('$id'(Id), Doc) -> update(Doc) ; update(['$id'(Id)|Doc])),
    reply_json(json([ status = success, data = Id ])).

% Removes the given document.
% Replies back id.
    
:- http_del(api/doc/Id, [ auth ], doc_remove(Id)).
    
doc_remove(Id, _):-
    remove(Id),
    reply_json(json([ status = success, data = Id ])).

% Logins into the system with username/password.
% When logic succeeds, sends user id back.
% Otherwise sends error 102.
    
:- http_post(api/login, login).

login(Req):-
    http_read_data(Req, json(Data), []),
    memberchk(username = User, Data),
    memberchk(password = Pass, Data),
    login_response(User, Pass).
    
login_response(User, Pass):-
    find(users, (username = User, password = Pass), [ Doc ]), !,
    prop_get('$id', Doc, Id),
    http_session_assert(user(Doc)),
    reply_json(json([ status = success, data = Id ])).

login_response(_, _):-
    reply_json(json([ status = error, code = 102 ])).
