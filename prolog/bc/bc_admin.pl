:- module(bc_admin, []).

:- use_module(library(http/http_dispatch)).

:- http_handler('/admin', serve_admin_file, [prefix]).

serve_admin_file(Request):-
    memberchk(path(Path), Request),
    (   Path = '/admin'
    ->  File = '/index.html'
    ;   atom_concat('/admin', File, Path)),
    public_path(Public),
    atom_concat(Public, File, Full),
    exists_file(Full),
    \+ sub_atom(Full, _, _, _, '..'),
    http_reply_file(Full, [unsafe(true)], Request).

public_path(Public):-
    module_property(bc_admin, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/public', Public).
