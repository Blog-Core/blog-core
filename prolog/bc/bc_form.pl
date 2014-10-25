:- module(bc_form, [
    bc_form_read/1
]).

/** <module> Module for reading form inputs from the current request */

:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_client)).

%! bc_form_read(-Dict) is semidet.
%
% Reads application/x-www-form-urlencoded data
% using http_read_data/3 and turns it into a dict.
% Fails when http_read_data/3 retrieves data not
% as a list.

bc_form_read(Dict):-
    http_current_request(Request),
    http_read_data(Request, Data, []),
    (   is_list(Data)
    ->  dict_create(Dict, _, Data)
    ;   fail).
