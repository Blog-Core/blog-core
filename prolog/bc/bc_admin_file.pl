:- module(bc_admin_file, [
    bc_admin_send_file/1, % +Path
    bc_admin_relative/2   % +Spec, -Path
]).

/** <module> Helper module to send admin-related files */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(bc_env).

bc_admin_send_file(Spec):-
    bc_admin_relative(Spec, Full),
    check_path(Full),
    send_file_unsafe(Full).

send_file_unsafe(Path):-
    bc_env_production, !,
    http_current_request(Request),
    get_time(TimeStamp),
    MaxAge is 365 * 24 * 60 * 60,
    Expire is TimeStamp + MaxAge,
    http_timestamp(Expire, ExpireString),
    atom_concat('max-age=', MaxAge, CacheControl),
    http_reply_file(Path,
        [unsafe(true), headers([
            cache_control(CacheControl),
            expires(ExpireString)])], Request).

% In development, send without strong caching headers.

send_file_unsafe(Path):-
    http_current_request(Request),
    http_reply_file(Path, [unsafe(true)], Request).

% Checks that the path is safe. It
% must not contain '..'.

check_path(Path):-
    sub_atom(Path, _, _, _, '..'), !,
    throw(error('Path must not contain ..')).

check_path(_).

% Turns admin-local URL path
% to absolute filesystem path.

bc_admin_relative(Spec, Path):-
    public_path(Public),
    spec_to_path(Public/Spec, Path).

spec_to_path(Atom, Atom):-
    atom(Atom), !.

spec_to_path(/(Prefix, Name), Path):-
    spec_to_path(Prefix, PrefixPath),
    spec_to_path(Name, NamePath),
    atom_concat(PrefixPath, '/', PrefixPathSlash),
    atom_concat(PrefixPathSlash, NamePath, Path).

public_path(Public):-
    module_property(bc_admin_file, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/public', Public).
