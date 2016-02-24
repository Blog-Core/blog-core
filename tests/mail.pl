:- begin_tests(mail).

:- use_module(prolog/bc/bc_mail_template).

test('Default mention template'):-
    bc_mail_render_template(mention, _{
        receiver: _{ name: 'User', comment_id: 'abc-123' },
        comment: _{ content: 'Test comment', post: 'abc-123' },
        entry: _{ title: 'Test entry' },
        entry_url: 'http://example.com/entry/test'
    }, Result),
    assertion(Result.subject = "Test entry - comment"),
    assertion(sub_string(Result.body, _, _, _, "Test comment")).

:- end_tests(mail).
