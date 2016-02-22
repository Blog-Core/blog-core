:- begin_tests(mail).

:- use_module(prolog/bc/bc_mail_template).

test('Default mention template'):-
    bc_mail_render_template(mention, _{
        receiver: _{ name: 'User' },
        comment: _{ content: 'Test comment' },
        entry: _{ title: 'Test entry' }
    }, Result),
    assertion(Result.subject = "Test entry - comment"),
    assertion(sub_string(Result.body, _, _, _, "Test comment")).

:- end_tests(mail).
