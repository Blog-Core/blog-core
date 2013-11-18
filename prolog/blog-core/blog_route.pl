:- module(blog_route, [
    blog_route/1,
    send_file/1,
    http_get/2,
    http_post/2,
    http_put/2,
    http_del/2,
    http_get/3,
    http_post/3,
    http_put/3,
    http_del/3
]).

:- use_module(library(http/mimetype)).
:- use_module(library(debug)).

:- dynamic(route/5).

:- module_transparent(http_get/2).
:- module_transparent(http_post/2).
:- module_transparent(http_put/2).
:- module_transparent(http_del/2).
:- module_transparent(http_get/3).
:- module_transparent(http_post/3).
:- module_transparent(http_put/3).
:- module_transparent(http_del/3).

http_get(Route, Goal):-
    http_get(Route, [], Goal).
    
http_put(Route, Goal):-
    http_put(Route, [], Goal).
    
http_del(Route, Goal):-
    http_del(Route, [], Goal).
    
http_post(Route, Goal):-
    http_post(Route, [], Goal).

http_get(Route, _, _):-
    blog_route:route(get, Route, _, _, _), !.
    
http_get(Route, Before, Goal):-
    context_module(Module),
    assertz(blog_route:route(get, Route, Module, Goal, Before)).
    
http_post(Route, _, _):-
    blog_route:route(post, Route, _, _, _), !.
    
http_post(Route, Before, Goal):-
    context_module(Module),
    assertz(blog_route:route(post, Route, Module, Goal, Before)).
    
http_put(Route, _, _):-
    blog_route:route(put, Route, _, _, _), !.
    
http_put(Route, Before, Goal):-
    context_module(Module),
    assertz(blog_route:route(put, Route, Module, Goal, Before)).
    
http_del(Route, _, _):-
    blog_route:route(delete, Route, _, _, _), !.
    
http_del(Route, Before, Goal):-
    context_module(Module),
    assertz(blog_route:route(delete, Route, Module, Goal, Before)).
    
tokens_term([/|Tokens], Term):-
    termize(Tokens, Term), !.
    
termize([], /).
    
termize([X|Rest], Term):-
    termize(Rest, X, Term).
    
termize([/,A|Rest], Acc, Term):-
    termize(Rest, '/'(Acc, A), Term).
    
termize([A], Acc, '/'(Acc, A)).

termize([], Acc, Acc).

tokens([Token|Tokens]) -->
    token(Token),
    tokens(Tokens).
    
tokens([]) --> [].

token(/) --> "/", !.

token(Atom) -->
    char(Char), !,
    char_token(Chars),
    { atom_chars(Atom, [Char|Chars]) }.

char_token([Char|Chars]) -->
    char(Char), !,
    char_token(Chars).
    
char_token([]) --> [].

char(Char) -->
    [Char], { Char \= 47 }.

blog_route(Request):-    
    memberchk(method(Method), Request),
    path(Request, Path),
    debug(blog_route, 'Dispatch: ~w ~w', [ Method, Path ]),
    dispatch(Method, Path, Request).
    
path(Request, Route):-
    memberchk(path(Atom), Request),
    atom_codes(Atom, Codes),
    phrase(tokens(Path), Codes),
    tokens_term(Path, Route).
    
dispatch(Method, Path, Request):-
    route(Method, Path, Module, Goal, Before), !,
    debug(blog_route, 'Module: ~w, Goal: ~w, Before: ~w', [ Module, Goal, Before ]),
    dispatch_route(Method, Path, Module, Goal, Before, Request).
    
dispatch(get, _, Request):-
    memberchk(path(Path), Request),
    \+ sub_atom(Path, _, _, _, '..'),
    atom_concat('/', FilePath, Path),
    atom_concat('public/', FilePath, FullPath),
    exists_file(FullPath), !,
    send_file(FullPath).
    
dispatch(_, _, Request):-
    memberchk(path(Path), Request),
    throw(http_reply(not_found(Path))).
    
dispatch_route(_, _, Module, Goal, Before, Request):-
    run_before(Before, Module, Goal, Request).
    
dispatch_route(Method, Path, _, _, _):-
    throw(error(route_failed(Method, Path))).
    
run_before([ Step|Rest ], Module, Goal, Request):- !,
    debug(blog_route, 'Calling: ~w', [ Step ]),
    call(Module:Step, blog_route:run_before(Rest, Module, Goal, Request), Request).
    
run_before([], Module, Goal, Request):-
    debug(blog_route, 'Calling handler: ~w', [ Goal ]),
    call(Module:Goal, Request), !,
    debug(blog_route, 'Handler finished', []).
    
send_file(File):-
    file_mime(File, Mime),
    throw(http_reply(file(Mime, File))).
    
file_mime(File, Mime):-
    file_mime_type(File, Mime), !.
    
file_mime(_, application/octet).
