:- module(bc_mail, [
    bc_mail_send/4,      % +Goal, +From, +Subject, +To
    bc_mail_send_text/4, % +Text, +From, +Subject, +To
    bc_mail_test/3       % +User, +Params, -Result
]).

/** <module> Helper module to send mail notifications */

:- use_module(library(smtp)).
:- use_module(library(error)).
:- use_module(library(debug)).

:- use_module(bc_data_config).

%! bc_mail_test(+User, +Params, -Result) is det.
%
% Tries to send a test email. Result is one
% of: error(E), fail, ok.

bc_mail_test(User, Params, Result):-
    must_be(dict, User),
    must_be(dict, Params),
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
        text_body(Params.body), Config, Result).

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

text_body(Text, Out):-
    writeln(Out, Text).

:- meta_predicate(bc_mail_send(1, +, +, +)).

%! bc_mail_send(:Goal, +From, +Subject, +To) is det.
%
% Sends mail using the current SMTP configuration.
% Takes Goal argument that must produce the mail
% body. Goal must accept argument for output Stream.

bc_mail_send(Goal, From, Subject, To):-
    must_be(atomic, From),
    must_be(atomic, Subject),
    must_be(atomic, To),
    must_be(callable, Goal),
    (   bc_config_get(smtp_enabled, true)
    ->  debug(bc_mail, 'smtp is enabled', []),
        smtp_config(Config),
        put_dict(_{ from: From, subject: Subject },
            Config, Options),
        wrap_smtp(To, Goal, Options, _)
    ;   debug(bc_mail, 'smtp is not enabled', [])).

%! bc_mail_send_text(+Text, +From, +Subject, +To) is det.
%
% Same as bc_mail_send/4 but takes prepared
% text instead of closure.

bc_mail_send_text(Text, From, Subject, To):-
    must_be(atomic, Text),
    bc_mail_send(text_body(Text), From, Subject, To).

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
