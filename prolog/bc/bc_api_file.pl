:- module(bc_api_file, []).

/** <module> HTTP handlers for file management
*/

:- use_module(library(http/http_wrapper)).
:- use_module(library(filesex)).
:- use_module(library(arouter)).

:- use_module(bc_hex).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_entry).
:- use_module(bc_access).
:- use_module(bc_files).

% Sends directory listing in public directory.

:- route_get(api/files/EntryId,
    bc_auth, files_get(EntryId)).

files_get(EntryId):-
    bc_actor(Actor),
    can_list(Actor, EntryId),
    bc_entry_slug(EntryId, Slug),
    atomic_list_concat([public, '/', Slug], Full),
    check_safe_path(Full),
    (   exists_directory(Full)
    ->  directory_only_files(Full, Files),
        maplist(file_record, Files, List)
    ;   List = []),
    bc_reply_success(List).

can_list(Actor, EntryId):-
    bc_entry_exists(EntryId),
    list_access(Actor, EntryId).

list_access(Actor, EntryId):-
    bc_read_access_id(Actor, EntryId), !.

list_access(_, _):-
    throw(error(no_access)).

% Turns file into a dict
% containing its metainfo.

file_record(File, _{ name: File }).

% Receives the uploaded file.

:- route_post(api/upload/EntryId,
    bc_auth, upload_file(EntryId)).

upload_file(EntryId):-
    bc_actor(Actor),
    can_upload(Actor, EntryId),
    catch(attemp_upload(EntryId), Error, true),
    (   var(Error)
    ;   (   drain_request,
            throw(Error))), !.

can_upload(Actor, EntryId):-
    bc_entry_exists(EntryId),
    upload_access(Actor, EntryId).

upload_access(Actor, EntryId):-
    bc_files_access_id(Actor, EntryId),
    bc_update_access_id(Actor, EntryId), !.

upload_access(_, _):-
    throw(error(no_access)).

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
    bc_entry_slug(EntryId, Slug),
    atomic_list_concat([public, '/', Slug], Directory),
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
    bc_actor(Actor),
    can_remove(Actor, EntryId),
    bc_entry_slug(EntryId, Slug),
    atomic_list_concat([public, '/', Slug, '/', Name], Full),
    check_safe_path(Full),
    delete_file(Full),
    bc_reply_success(true).

can_remove(Actor, EntryId):-
    bc_entry_exists(EntryId),
    upload_access(Actor, EntryId).

% Checks that the given path is safe to be used.

check_safe_path(Path):-
    (   sub_atom(Path, _, _, _, '..')
    ->  throw(error(unsafe_path(Path)))
    ;   true).
