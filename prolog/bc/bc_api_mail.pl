:- module(bc_api_mail, []).

/** <module> HTTP handlers for mail system */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_mail).

% Tests the mail system with the posted
% parameters.

:- route_post(api/mail/test,
    bc_auth, mail_test).

mail_test:-
    bc_actor(Actor),
    bc_read_by_schema(bc_mail_test, Params),
    bc_mail_test(Actor, Params, Result),
    (   Result = ok
    ->  bc_reply_success(true)
    ;   (   Result = error(E)
        ->  message_to_string(E, String),
            bc_reply_error(String)
        ;   (   Result = fail
            ->  bc_reply_error('Sending failed')
            ;   bc_reply_error('Unknown result')))).

% Generic config entry.

:- register_schema(bc_mail_test, _{
    type: dict,
    keys: _{
        host: atom,
        user: atom,
        password: atom,
        auth: atom,
        security: atom,
        from: atom,
        subject: atom,
        body: atom
    },
    additional: true
}).
