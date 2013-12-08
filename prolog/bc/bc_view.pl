:- module(bc_view, [
    send_view/2 % +Name, +Data
]).

:- use_module(library(st/st_render)).

% Only enable template cache
% in production environment.

:- if(getenv('PL_ENV', production)).
    :- st_enable_cache(true).
:- endif.

send_view(Name, Data):-
    format('Content-type: text/html; charset=UTF-8~n~n'),
    st_render_file(Name, Data).
