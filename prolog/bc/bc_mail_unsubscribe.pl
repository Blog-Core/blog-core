:- module(bc_mail_unsubscribe, []).

/** <module> HTTP handlers for mail unsubscription */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_data_mail).

% Unsubscribes from single entry
% comments.

:-  route_get(mail/unsubscribe/entry/CommentId,
    mail_unsubscribe_entry(CommentId)).

mail_unsubscribe_entry(CommentId):-
    bc_mail_unsubscribe_entry(CommentId),
    write_response.

% Unsubscribes from all comments.

:-  route_get(mail/unsubscribe/all/CommentId,
    mail_unsubscribe_all(CommentId)).

mail_unsubscribe_all(CommentId):-
    bc_mail_unsubscribe_all(CommentId),
    write_response.

% FIXME use bc_view.

write_response:-
    write('Content-type: text/plain; charset=UTF-8\r\n\r\n'),
    write('You have been unsubscribed.').
