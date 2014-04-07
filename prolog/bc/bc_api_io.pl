:- module(bc_api_io, [
    bc_reply_success/1, % +Dict
    bc_reply_error/1,   % +Message
    bc_read_by_schema/2 % +Schema, -Dict
]).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(dict_schema)).

%! bc_read_by_schema(+Schema, -Dict) is det.
%
% Reads dict from JSON request
% and validates it against the schema.
% Throws error(invalid_input(Errors)) when
% input data contains validation errors.

bc_read_by_schema(Schema, Dict):-
    http_current_request(Request),
    http_read_json_dict(Request, Raw),
    convert(Raw, Schema, Dict, Errors),
    (   Errors = []
    ;   throw(error(invalid_input(Errors)))), !.

%! bc_reply_success(+Data) is det.
%
% Sends JSON response with Data
% and success status.

bc_reply_success(Data):-
    reply_json(_{ status: success, data: Data }).

%! bc_reply_error(+Message) is det.
%
% Sends error JSON response with Message.

bc_reply_error(Message):-
    reply_json(_{ status: error, message: Message }).
