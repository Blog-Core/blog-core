:- begin_tests(mentions).

:- use_module(prolog/bc/bc_comment_mention).

test('No content'):-
    bc_mentions_parse("", ["someone", "someone else"], Mentions),
    assertion(Mentions = []).

test('One mention 1'):-
    bc_mentions_parse("@someone", ["someone", "someone else"], Mentions),
    assertion(Mentions = ["someone"]).

test('One mention 2'):-
    bc_mentions_parse("text @someone, more text", ["someone", "someone else"], Mentions),
    assertion(Mentions = ["someone"]).

test('Whitespace in name'):-
    bc_mentions_parse("text @someone else, more text", ["someone else"], Mentions),
    assertion(Mentions = ["someone else"]).

test('No content, no mentions'):-
    bc_mentions_parse("", [], Mentions),
    assertion(Mentions = []).

test('No content, no names'):-
    bc_mentions_parse("some text @someone", [], Mentions),
    assertion(Mentions = []).

:- end_tests(mentions).
