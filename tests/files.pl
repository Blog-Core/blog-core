:- begin_tests(files).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(readutil)).

:- use_module(prolog/bc/bc_bust).

% Reads URL contents to string.

request_get_file(Path, Content):-
    atom_concat('http://localhost:18008', Path, Url),
    http_open(Url, Stream, []),
    read_string(Stream, _, Content),
    close(Stream).

test('Download file'):-
    request_get_file('/test.txt', Content),
    assertion(Content = "hello").

test('Download file through token'):-
    bc_bust_token(Token),
    atomic_list_concat(['/t-', Token, '/test.txt'], Path),
    request_get_file(Path, Content),
    assertion(Content = "hello").

:- end_tests(files).
