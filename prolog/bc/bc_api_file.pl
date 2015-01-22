:- module(bc_api_file, []).

/** <module> HTTP handlers for file management
*/

:- use_module(library(http/http_wrapper)).
:- use_module(library(filesex)).
:- use_module(library(arouter)).

:- use_module(bc_hex).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_cur_user).
:- use_module(bc_access).

% Sends directory listing in public directory.

:- route_get(api/files/EntryId,
    bc_auth, files_get(EntryId)).

files_get(EntryId):-
    bc_user(Actor),
    can_list(Actor, EntryId),
    atomic_list_concat([public, '/', EntryId], Full),
    check_safe_path(Full),
    (   exists_directory(Full)
    ->  directory_files(Full, Entries),
        entry_records(Entries, Full, List)
    ;   List = []),
    bc_reply_success(List).

can_list(Actor, EntryId):-
    bc_entry_exists(EntryId),
    bc_type_access_by_id(Actor, read, EntryId).

% Finds directory entries and turns
% them into dicts having `name` and
% `directory` keys.
% FIXME only files

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

% Receives the uploaded file.

:- route_post(api/upload/EntryId,
    bc_auth, upload_file(EntryId)).

upload_file(EntryId):-
    bc_user(Actor),
    can_upload(Actor, EntryId),
    catch(attemp_upload(EntryId), Error, true),
    (   var(Error)
    ;   (   drain_request,
            throw(Error))), !.

can_upload(Actor, EntryId):-
    bc_entry_exists(EntryId),
    bc_type_access_by_id(Actor, update, EntryId),
    bc_ownership_by_id(Actor, EntryId),
    bc_files_access(Actor).

% Drains the remaining data
% from the request body.

drain_request:-
    http_current_request(Request),
    memberchk(input(In), Request),
    setup_call_cleanup(
        open_null_stream(Null),
        (   memberchk(content_length(Len), Request)
        ->  copy_stream_data(In, Null, Len)
        ;   copy_stream_data(In, Null)),
        close(Null)).

% Runs the actual upload process.
% Creates the entry directory if
% it does not exist.

attemp_upload(EntryId):-
    http_current_request(Request),
    memberchk(x_file_name(Target), Request),
    atomic_list_concat([public, '/', EntryId], Directory),
    check_safe_path(Directory),
    (   exists_directory(Directory)
    ->  true
    ;   make_directory(Directory)),
    atomic_list_concat([Directory, '/', Target], Full),
    check_safe_path(Full),
    memberchk(input(In), Request),
    (   exists_file(Full)
    ->  throw(error(file_exists))
    ;   true),
    (   exists_directory(Full)
    ->  throw(error(directory_exists))
    ;   true),
    setup_call_cleanup(
        open(Full, write, Stream, [encoding(octet)]),
        (   memberchk(content_length(Len), Request)
        ->  copy_stream_data(In, Stream, Len)
        ;   copy_stream_data(In, Stream)),
        close(Stream)),
    bc_reply_success(true).

% Removes the given file.

:- route_del(api/file/EntryId/Name,
    bc_auth, file_remove(EntryId, Name)).

file_remove(EntryId, Name):-
    bc_user(Actor),
    can_remove(Actor, EntryId),
    atomic_list_concat([public, '/', EntryId, '/', Name], Full),
    check_safe_path(Full),
    delete_file(Full),
    bc_reply_success(true).

can_remove(Actor, EntryId):-
    bc_entry_exists(EntryId),
    bc_type_access_by_id(Actor, update, EntryId),
    bc_ownership_by_id(Actor, EntryId),
    bc_files_access(Actor).

% Checks that the given path is safe to be used.

check_safe_path(Path):-
    (   sub_atom(Path, _, _, _, '..')
    ->  throw(error(unsafe_path(Path)))
    ;   true).
