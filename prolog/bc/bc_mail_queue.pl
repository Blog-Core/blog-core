:- module(bc_mail_queue, [
    bc_mail_enqueue_text/4, % +Text, +From, +Subject, +To
    bc_mail_queue_wipe/0,
    bc_mail_queue_size/1,   % -Size
    bc_mail_set_behavior/1  % +Behavior
]).

:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(docstore)).

:- use_module(bc_mail).

% Queue behavior: ignore, retain, send.
% Production value is send.

:- dynamic(behavior/1).
:- dynamic(queue/5).

% Default is used for testing.

:- assertz(behavior(retain)).

bc_mail_set_behavior(Behavior):-
    must_be(oneof([retain, send, ignore]), Behavior),
    retractall(behavior(_)),
    assertz(behavior(Behavior)).

bc_mail_enqueue_text(Text, From, Subject, To):-
    must_be(atomic, Text),
    must_be(atomic, From),
    must_be(atomic, Subject),
    must_be(atomic, To),
    (   behavior(ignore)
    ->  true
    ;   ds_uuid(Uuid),
        assertz(queue(Uuid, Text, From, Subject, To)),
        debug(bc_mail, 'queued mail to ~w', [To])).

bc_mail_queue_wipe:-
    retractall(queue(_, _, _, _, _)),
    debug(bc_mail, 'wiping mail queue', []).

bc_mail_queue_size(Size):-
    findall(_, queue(_, _, _, _, _), Mails),
    length(Mails, Size).

% Sleep time setting for the
% mail queue thread.

queue_thread_sleep(10).

start_mail_thread:-
    debug(bc_mail, 'started queue thread', []),
    queue_thread_sleep(Sleep),
    sleep(Sleep),
    queue_loop.

% Tail-call optimized loop.

queue_loop:-
    queue_loop_iteration,
    queue_thread_sleep(Sleep),
    sleep(Sleep),
    queue_loop.

queue_loop_iteration:-
    (   behavior(send)
    ->  send_all_mails
    ;   true).

% Queue loop thread is always started.

:- thread_create(start_mail_thread, _, []).

% Sends all mails.

send_all_mails:-
    findall(
        queue(Uuid, Text, From, Subject, To),
        queue(Uuid, Text, From, Subject, To), Mails),
    length(Mails, Count),
    debug(bc_mail, 'sending ~w mails from queue', [Count]),
    maplist(send_mail, Mails).

% Sends one mail.

send_mail(Mail):-
    Mail = queue(Uuid, Text, From, Subject, To),
    debug(bc_mail, 'sending mail to ~w', [To]),
    retractall(queue(Uuid, _, _, _, _)),
    bc_mail_send_text(Text, From, Subject, To).
