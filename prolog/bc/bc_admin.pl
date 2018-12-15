:- module(bc_admin, []).

/** <module> HTTP handlers for administration interface */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(arouter)).

:- use_module(bc_env).
:- use_module(bc_view).
:- use_module(bc_main).
:- use_module(bc_data_config).
:- use_module(bc_admin_file).

:- route_get(admin/css/File,
   bc_admin_send_file(css/File)).

:- route_get(admin/fonts/File,
   bc_admin_send_file(fonts/File)).

:- route_get(admin/img/File,
   bc_admin_send_file(img/File)).

:- route_get(admin/js/File,
   bc_admin_send_file(js/File)).

:- route_get(admin/js/libs/File,
   bc_admin_send_file(js/libs/File)).

:- route_get(admin/js/libs/ace/File,
   bc_admin_send_file(js/libs/ace/File)).

:- route_get(admin, send_admin).

:- route_get(admin/unsupported, send_unsupported).

% Renders the main admin HTML page.
% Provides it configuration info.

send_admin:-
    bc_env(Env),
    bc_config_get(default_language, Lang),
    bc_config_get(site, Site),
    bc_admin_relative(index, Full),
    pack_property(blog_core, version(Version)),
    bc_view_send(Full, _{
        environment: Env,
        language: Lang,
        version: Version,
        site: Site
    }).

% Server the page that is shown to
% unsupported browsers.

send_unsupported:-
    bc_admin_relative(unsupported, Full),
    bc_view_send(Full, _{}).
