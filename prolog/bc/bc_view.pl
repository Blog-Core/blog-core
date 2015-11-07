:- module(bc_view, [
    bc_view_cached/1,       % +Path
    bc_view_send/2,         % +Name, +Data
    bc_view_send/3,         % +Name, +Data, +ContentType
    bc_view_enable_cache/0,
    bc_view_disable_cache/0,
    bc_view_purge_cache/0,
    bc_view_not_found/0,
    bc_view_see_other/1     % + URL
]).

:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(debug)).
:- use_module(library(st/st_file)).
:- use_module(library(st/st_render)).

:- use_module(bc_headers).

:- dynamic(cache_enabled/0).
:- dynamic(cache/4).

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
    retractall(cache(_, _, _, _)),
    debug(bc_view, 'view caching is disabled', []).

%! bc_view_purge_cache is det.
%
% Purges all cache entries.

bc_view_purge_cache:-
    retractall(cache(_, _, _, _)),
    debug(bc_view, 'purged cache', []).

%! bc_view_not_found is det.
%
% Sends non-cached 404 response.
% Uses exceptions mechanism to produce the response.

bc_view_not_found:-
    http_current_request(Request),
    http_404([], Request).

%! bc_view_see_other(+Url) is det.
%
% Sends redirect 303 (see other) for
% the current request.

bc_view_see_other(Url):-
    http_current_request(Request),
    http_redirect(see_other, Url, Request).

%! bc_view_cached(+Path, +Content) is semidet.
%
% Produces reply from cached view. Fails when
% there is no cached result for the URL path.

bc_view_cached(Path):-
    cache(Path, Content, Type, Time),
    http_current_request(Request),
    (   bc_if_modified_since(Request, Since),
        Since >= Time
    ->  debug(bc_view, 'sending not-modified status for ~p', [Path]),
        throw(http_reply(not_modified))
    ;   debug(bc_view, 'sending cached view for ~p', [Path]),
        get_time(Now),
        write_cache_control_public,
        write_last_modified(Now),
        write_content_type(Type),
        write(Content)).

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
    get_time(Now),
    write_cache_control_public,
    write_last_modified(Now),
    write_content_type(Type),
    with_output_to(string(Content),
        render_with_options(Name, Data)),
    asserta(cache(Path, Content, Type, Now)),
    debug(bc_view, 'stored view in cache ~p', [Path]),
    write(Content).

bc_view_send(Name, Data, Type):-
    write_content_type(Type),
    render_with_options(Name, Data).

% FIXME use cache option from bc_environment.

render_with_options(Name, Data):-
    current_output(Stream),
    st_render_file(Name, Data, Stream,
        _{ encoding: utf8, strip: true,
           cache: true, extension: html }).

% The default content type for views.

default_content_type('Content-type: text/html; charset=UTF-8').

%! write_content_type(+Type) is det.
%
% Writes the given content type and charset.
% Does not validate anything.

write_content_type(Type):-
    format('~w\r\n\r\n', [Type]).

%! write_cache_control_public is det.
%
% Writes Cache-control: public header.

write_cache_control_public:-
    write('Cache-Control: public\r\n').

%! write_last_modified(+Timestamp) is det.
%
% Writes Last-Modified header
% based on the given timestamp.

write_last_modified(Timestamp):-
    http_timestamp(Timestamp, String),
    format('Last-Modified: ~w\r\n', [String]).
