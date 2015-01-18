:- module(bc_api_file, []).

/** <module> HTTP handlers for file management
*/

:- use_module(library(http/http_wrapper)).
:- use_module(library(filesex)).
:- use_module(library(arouter)).

:- use_module(bc_hex).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).

% Sends directory listing in public directory.

:- route_get(api/directory/Hex,
    bc_auth, directory_get(Hex)).

directory_get(Hex):-
    bc_hex_atom(Hex, Path),
    atom_concat(public, Path, Full),
    check_safe_path(Full),
    directory_files(Full, Entries),
    entry_records(Entries, Full, List),
    bc_reply_success(List).

% Finds directory entries and turns
% them into dicts having `name` and
% `directory` keys.

entry_records(['.'|Entries], Dir, Records):- !,
    entry_records(Entries, Dir, Records).

entry_records(['..'|Entries], Dir, Records):- !,
    entry_records(Entries, Dir, Records).

entry_records([Entry|Entries], Dir, [Record|Records]):-
    atomic_list_concat([Dir, '/', Entry], File),
    (   exists_directory(File)
    ->  Directory = true
    ;   Directory = false),
    Record = _{ name: Entry, directory: Directory },
    entry_records(Entries, Dir, Records).

entry_records([], _, []).

% Creates new subdirectory in the given path.

:- route_post(api/directory/Hex/Sub,
    bc_auth, directory_new(Hex, Sub)).

directory_new(Hex, Sub):-
    bc_hex_atom(Hex, Path),
    atomic_list_concat([public, Path, '/', Sub], Full),
    check_safe_path(Full),
    make_directory(Full),
    bc_reply_success(true).

% Receives the uploaded file.

:- route_post(api/upload/Hex,
    bc_auth, upload_file(Hex)).

upload_file(Hex):-
    bc_hex_atom(Hex, Path),
    http_current_request(Request),
    memberchk(x_file_name(Target), Request),
    atomic_list_concat([public, Path, '/', Target], Full),
    check_safe_path(Full),
    memberchk(input(In), Request),
    setup_call_cleanup(
        open(Full, write, Stream, [encoding(octet)]),
        (   memberchk(content_length(Len), Request)
        ->  copy_stream_data(In, Stream, Len)
        ;   copy_stream_data(In, Stream)),
        close(Stream)),
    bc_reply_success(true).

% Removes the given directory.

:- route_del(api/directory/Hex,
    bc_auth, directory_remove(Hex)).

directory_remove(Hex):-
    bc_hex_atom(Hex, Path),
    atom_concat(public, Path, Full),
    check_safe_path(Full),
    delete_directory_rec(Full),
    bc_reply_success(true).

% Recursively removes the directory.

delete_directory_rec(Path):-
    directory_files(Path, Entries),
    delete_directory_entries(Entries, Path),
    delete_directory(Path).

delete_directory_entries(['.'|Entries], Path):- !,
    delete_directory_entries(Entries, Path).

delete_directory_entries(['..'|Entries], Path):- !,
    delete_directory_entries(Entries, Path).

delete_directory_entries([Entry|Entries], Path):-
    atomic_list_concat([Path, '/', Entry], File),
    (   exists_directory(File)
    ->  delete_directory_rec(File)
    ;   delete_file(File)),
    delete_directory_entries(Entries, Path).

delete_directory_entries([], _).

% File metainfo.

:- route_get(api/file/Hex,
    bc_auth, file_get(Hex)).

file_get(Hex):-
    bc_hex_atom(Hex, Path),
    atom_concat(public, Path, Full),
    check_safe_path(Full),
    set_time_file(Full, [modified(Time)], []),
    Ts is floor(Time),
    size_file(Full, Size),
    bc_reply_success(_{ modified: Ts, size: Size }).

% Removes the given file.

:- route_del(api/file/Hex,
    bc_auth, file_remove(Hex)).

file_remove(Hex):-
    bc_hex_atom(Hex, Path),
    atom_concat(public, Path, Full),
    check_safe_path(Full),
    delete_file(Full),
    bc_reply_success(true).

% Checks that the given path is safe to be used.

check_safe_path(Path):-
    (   sub_atom(Path, _, _, _, '..')
    ->  throw(error(unsafe_path(Path)))
    ;   true).
