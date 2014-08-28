:- module(util_comment, [
    new_comment/2,   % +PostId, -Response
    new_comment/3,   % +PostId, +ReplyTo, -Response
    list_comments/2  % +PostId, -Response
]).

:- use_module(util).

new_comment(PostId, Response):-
    atomic_list_concat(['/api/post/', PostId, '/comment'], Path),
    request_post(Path, _{
        author: "RLa",
        content: "Test comment",
        question: 1,
        answer: "3"
    }, Response).

new_comment(PostId, ReplyTo, Response):-
    atomic_list_concat(['/api/post/', PostId, '/comment'], Path),
    request_post(Path, _{
        author: "RLa",
        content: "Test comment",
        question: 1,
        answer: "3",
        reply_to: ReplyTo
    }, Response).

list_comments(PostId, Response):-
    atomic_list_concat(['/api/post/', PostId, '/comments'], Path),
    request_get(Path, Response).
