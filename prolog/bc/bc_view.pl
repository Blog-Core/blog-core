:- module(bc_view, [
    bc_view_cached/1,       % +Path
    bc_view_send/2,         % +Name, +Data
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

:- dynamic(cache_enabled).

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
    retractall(cache(_, _)),
    debug(bc_view, 'view caching is disabled', []).

%! bc_view_purge_cache is det.
%
% Purges all cache entries.

bc_view_purge_cache:-
    retractall(cache(_, _)),
    debug(bc_view, 'purged cache', []).

:- dynamic(cache/2).

%! bc_view_cached(+Path, +Content) is semidet.
%
% Produces reply from cached view. Fails when
% there is no caches result for the URL path.

bc_view_cached(Path):-
    cache(Path, Content),
    debug(bc_view, 'sending cached view for ~p', [Path]),
    write_content_type,
    write(Content).

%! bc_view_send(+Name, +Data) is det.
%
% Renders and sends a simple-template view.
% Stores rendering result in cache when caching
% is enabled.

% FIXME version with content-type

bc_view_send(Name, Data):-
    cache_enabled, !,
    http_current_request(Request),
    memberchk(path(Path), Request),
    write_content_type,
    with_output_to(string(Content), st_render_file(Name, Data)),
    asserta(cache(Path, Content)),
    debug(bc_view, 'stored view in cache ~p', [Path]),
    write(Content).

bc_view_send(Name, Data):-
    write_content_type,
    st_render_file(Name, Data).

% Writes content type header.

write_content_type:-
    write('Content-type: text/html; charset=UTF-8\r\n\r\n').
