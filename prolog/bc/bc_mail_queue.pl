:- module(bc_mail_queue, [
    bc_mail_enqueue_text/4, % +Text, +From, +Subject, +To
    bc_mail_queue_wipe/0,
    bc_mail_queue_size/1    % -Size
]).

:- use_module(library(debug)).

:- dynamic(queue/4).

bc_mail_enqueue_text(Text, From, Subject, To):-
    assertz(queue(Text, From, Subject, To)),
    debug(bc_mail, 'queued mail to ~w', [To]).

bc_mail_queue_wipe:-
    retractall(queue(_, _, _, _)),
    debug(bc_mail, 'wiping mail queue', []).

bc_mail_queue_size(Size):-
    findall(_, queue(_, _, _, _), Mails),
    length(Mails, Size).
