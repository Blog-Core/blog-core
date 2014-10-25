:- module(bc_mail, [
    bc_mail_send/4
]).

/** <module> Helper module to send mail notifications */

:- use_module(library(smtp)).
:- use_module(bc_data_config).

:- meta_predicate(bc_mail_send(1, +, +, +)).

bc_mail_send(Goal, From, Subject, To):-
    (   bc_config_get(smtp_enabled, true)
    ->  smtp_config(Config),
        put_dict(_{ from: From, subject: Subject },
            Config, Options),
        smtp_send_mail(To, Goal, Options)
    ;   true).

% Builds dict from the current
% SMTP config options.

smtp_config(Config):-
    bc_config_get(smtp_host, Host),
    bc_config_get(smtp_auth, Auth),
    (   Auth = plain
    ->  Config = _{ smtp: Host, auth_method: Auth }
    ;   bc_config_get(smtp_user, User),
        bc_config_get(smtp_password, Password),
        Config = _{ smtp: Host, auth_method: Auth, auth: User-Password }).
