:- module(bc_admin, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(arouter)).

:- use_module(bc_view).
:- use_module(bc_main).
:- use_module(bc_data_config).

:- route_get(admin/css/File, send_file(css/File)).

:- route_get(admin/fonts/File, send_file(fonts/File)).

:- route_get(admin/img/File, send_file(img/File)).

:- route_get(admin/js/File, send_file(js/File)).

:- route_get(admin/js/shim/File, send_file(js/shim/File)).

:- route_get(admin/js/libs/File, send_file(js/libs/File)).

:- route_get(admin/js/libs/ace/File, send_file(js/libs/ace/File)).

:- route_get(admin, send_admin).

% Renders the main admin HTML page.
% Provides it configuration info.

send_admin:-
    bc_environment(Env),
    bc_config_get(default_language, Lang),
    admin_relative(index, Full),
    pack_property(blog_core, version(Version)),
    bc_view_send(Full, _{
        environment: Env,
        language: Lang,
        version: Version
    }).

send_file(Spec):-
    admin_relative(Spec, Full),
    check_path(Full),
    send_file_unsafe(Full).

send_file_unsafe(Path):-
    http_current_request(Request),
    http_reply_file(Path, [unsafe(true)], Request).

% Checks that the path is safe. It
% must not contain '..'.

check_path(Path):-
    sub_atom(Path, _, _, _, '..'), !,
    throw(error('Admin path must not contain ..')).

check_path(_).

% Turns admin-local URL path
% to absolute filesystem path.

admin_relative(Spec, Path):-
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
    module_property(bc_admin, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/public', Public).
