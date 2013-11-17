:- module(blog_admin, []).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_client)).
:- use_module(library(debug)).
:- use_module(library(docstore)).
:- use_module(blog_util).
:- use_module(blog_route).
:- use_module(blog_json).

:- http_get(admin, reply_file('admin.html')).

:- http_get(admin / File, reply_file(File)).

reply_file(File, _):-
    module_dir(blog_admin, ModDir),
    atomic_list_concat([ ModDir, '/public/', File ], FullPath),
    send_file(FullPath).
    
% Gives all documents in the collection.
    
:- http_get(api / col / Col, doc_all(Col)).

doc_all(Col, _):-
    all(Col, Docs),
    maplist(doc_to_json, Docs, JSONDocs),
    send_json(json([ status(success), data(JSONDocs) ])).

% Stores new document in the collection.
% Replies back id.

:- http_post(api / col / Col / doc, doc_insert(Col)).

doc_insert(Col, Req):-
    http_read_data(Req, JSONDoc, []),
    json_to_doc(JSONDoc, Doc),
    insert(Col, Doc, Id),
    send_json(json([ status(success), data(Id) ])).
    
% Gives document type by collection name.

:- http_get(api / col / Col / type, col_type(Col)).

col_type(Col, _):-
    find(types, name = Col, [Doc]),
    doc_to_json(Doc, JSONDoc),
    send_json(json([ status(success), data(JSONDoc) ])).
    
% Gives single document by id.

:- http_get(api / doc / Id, doc_get(Id)).

doc_get(Id, _):-
    get(Id, Doc),
    doc_to_json(Doc, JSONDoc),
    send_json(json([ status(success), data(JSONDoc) ])).
    
% Gives document type by document id.
    
:- http_get(api / doc / Id / type, doc_type(Id)).
    
doc_type(Id, _):-
    collection(Id, Col),
    find(types, name = Col, [Doc]),
    doc_to_json(Doc, JSONDoc),
    send_json(json([ status(success), data(JSONDoc) ])).
    
% Updates document by id.

:- http_put(api / doc / Id, doc_update(Id)).

doc_update(Id, Req):-
    http_read_data(Req, JSONDoc, []),
    json_to_doc(JSONDoc, Doc),
    (memberchk('$id'(Id), Doc) -> update(Doc) ; update(['$id'(Id)|Doc])),
    send_json(json([ status(success), data(Id) ])).

% Removes the given document.
% Replies back id.
    
:- http_del(api / doc / Id, doc_remove(Id)).
    
doc_remove(Id, _):-
    remove(Id),
    send_json(json([ status(success), data(Id) ])).
