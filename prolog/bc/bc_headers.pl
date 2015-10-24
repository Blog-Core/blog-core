:- module(bc_headers, [
    bc_if_modified_since/2 % +Request, -Time
]).

%! bc_if_modified_since(+Request, -Time) is semidet.
%
% Extracts and parses If-Modified-Since header
% from the request. Fails when the request
% contains no such header.

bc_if_modified_since(Request, Time):-
    memberchk(if_modified_since(Text), Request),
    parse_time(Text, rfc_1123, Time).
