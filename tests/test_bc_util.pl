:- begin_tests(bc_util).
:- use_module(prolog/bc/bc_util).

test(excerpt_length):-
    excerpt('<strong>abc</strong>def<br>ghi', 6, 'abcdef').

:- end_tests(bc_util).
