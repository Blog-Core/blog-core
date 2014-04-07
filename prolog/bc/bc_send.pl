:- module(bc_send, [
    bc_send_404/0
]).

/** <module> Helper HTTP response predicates

The module provides HTTP predicates to send
common responses.
*/

%! bc_send_404 is det.
%
% Helper to send HTTP 404 Not Found response.

bc_send_404:-
    http_current_request(Request),
    http_404([], Request).
