:- module(bc_mail, [
    bc_mail_send/4,
    bc_mail_test/3 % +User, +Params, -Result
]).

/** <module> Helper module to send mail notifications */

:- use_module(library(smtp)).
:- use_module(bc_data_config).

% Tries to send a test email.

bc_mail_test(User, Params, Result):-
    BaseConfig = _{
        smtp: Params.host,
        from: Params.from,
        subject: Params.subject,
        auth_method: Params.auth,
        security: Params.security
    },
    (   Params.auth = login
    ->  Config = BaseConfig.put(auth, Params.user - Params.password)
    ;   Config = BaseConfig),
    wrap_smtp(User.username,
        bc_mail_text_body(Params.body), Config, Result).

:- meta_predicate(wrap_smtp(+, 1, +, -)).

% Wrapper around smtp_send_mail to catch
% and report the mail error.

wrap_smtp(To, Goal, Config, Result):-
    (   catch(smtp_send_mail(To, Goal, Config), E, true)
    ->  (   nonvar(E)
        ->  format(user_error, 'Mail sending error: ~w~n', [E]),
            Result = error(E)
        ;   Result = ok)
    ;   writeln(user_error, 'Mail sending failed.'),
        Result = fail).

% Helper to write given
% text to output.

bc_mail_text_body(Text, Out):-
    writeln(Out, Text).

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
