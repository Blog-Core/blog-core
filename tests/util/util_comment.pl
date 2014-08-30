:- module(util_comment, [
    new_comment/2,    % +PostId, -Response
    new_comment/3,    % +PostId, +Override, -Response
    remove_comment/2, % +CommentId, -Response
    list_comments/2,  % +PostId, -Response,
    get_question/1    % -Response
]).

:- use_module(util).

% Sames as new_comment/3 but with no overrides.

new_comment(PostId, Response):-
    new_comment(PostId, _{}, Response).

% Saves new test comment. Accepts Override for
% default values.

new_comment(PostId, Override, Response):-
    atomic_list_concat(['/api/post/', PostId, '/comment'], Path),
    Base = _{
        author: "RLa",
        content: "Test comment",
        question: 1,
        answer: "3" },
    put_dict(Override, Base, Data),
    request_post(Path, Data, Response).

remove_comment(CommentId, Response):-
    atomic_list_concat(['/api/comment/', CommentId], Path),
    request_del(Path, Response).

list_comments(PostId, Response):-
    atomic_list_concat(['/api/post/', PostId, '/comments'], Path),
    request_get(Path, Response).

get_question(Response):-
    request_get('/api/question', Response).
