:- module(bc_view, [
    bc_view_cached/1,       % +Path
    bc_view_send/2,         % +Name, +Data
    bc_view_send/3,         % +Name, +Data, +ContentType
    bc_view_enable_cache/0,
    bc_view_disable_cache/0,
    bc_view_purge_cache/0
]).

:- use_module(library(http/http_wrapper)).
:- use_module(library(debug)).
:- use_module(library(st/st_file)).
:- use_module(library(st/st_render)).

% Sets file extension to be `.html`.

:- st_set_extension(html).

:- dynamic(cache_enabled/0).
:- dynamic(cache/3).

%! bc_view_enable_cache is det.
%
% Enables view caching.

bc_view_enable_cache:-
    asserta(cache_enabled),
    debug(bc_view, 'view caching is enabled', []).

%! bc_view_disable_cache is det.
%
% Disables view caching. Purges all cache entries.

bc_view_disable_cache:-
    retractall(cache_enabled),
    retractall(cache(_, _, _)),
    debug(bc_view, 'view caching is disabled', []).

%! bc_view_purge_cache is det.
%
% Purges all cache entries.

bc_view_purge_cache:-
    retractall(cache(_, _, _)),
    debug(bc_view, 'purged cache', []).

%! bc_view_cached(+Path, +Content) is semidet.
%
% Produces reply from cached view. Fails when
% there is no caches result for the URL path.

bc_view_cached(Path):-
    cache(Path, Content, Type),
    debug(bc_view, 'sending cached view for ~p', [Path]),
    write_content_type(Type),
    write(Content).

%! bc_view_send(+Name, +Data) is det.
%
% Same as bc_view_send/3 with the default
% content type. The default content type is
% `Content-type: text/html; charset=UTF-8`.

bc_view_send(Name, Data):-
    default_content_type(Type),
    bc_view_send(Name, Data, Type).

%! bc_view_send(+Name, +Data, +ContentType) is det.
%
% Renders and sends a simple-template view.
% Stores rendering result in cache when caching
% is enabled.

bc_view_send(Name, Data, Type):-
    cache_enabled, !,
    http_current_request(Request),
    memberchk(path(Path), Request),
    write_content_type(Type),
    with_output_to(string(Content), st_render_file(Name, Data)),
    asserta(cache(Path, Content, Type)),
    debug(bc_view, 'stored view in cache ~p', [Path]),
    write(Content).

bc_view_send(Name, Data, Type):-
    write_content_type(Type),
    st_render_file(Name, Data).

% The default content type for views.

default_content_type('Content-type: text/html; charset=UTF-8').

%! write_content_type(+Type) is det.
%
% Writes the given content type and charset.
% Does not validate anything.

write_content_type(Type):-
    format('~w\r\n\r\n', [Type]).
