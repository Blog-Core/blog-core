:- module(bc_comment, [
    bc_comment_list/2,      % +PostId, -Comments
    bc_comment_list_full/2, % +PostId, -Comments
    bc_comment_save/2,      % +PostId, +Comment
    bc_comment_remove/1,    % +Id
    bc_random_question/2    % -Id, -Question
]).

/** <module> Commenting support

Comments are checked for spam by asking questions
that are not easy to answer by a computer. This
module handles questions.
*/

:- use_module(library(http/html_write)).
:- use_module(library(md/md_span)).
:- use_module(library(sort_dict)).
:- use_module(library(docstore)).

:- use_module(bc_walk).

%! bc_comment_list(+PostId, -Comments) is det.
%
% Retrieves the list of comments
% for the given post.

bc_comment_list(PostId, Sorted):-
    ds_find(comment, post=PostId, Comments),
    sort_dict(date, desc, Comments, Sorted).

%! bc_comment_list_full(+PostId, -Comments) is det.
%
% Retrieves the list of comments.
% Includes full comment data. Sorts by date.

bc_comment_list_full(PostId, Sorted):-
    ds_find(comment, post=PostId, Comments),
    sort_dict(date, desc, Comments, Sorted).

%! bc_comment_save(+PostId, +Comment) is det.
%
% Saves the comment. Throws error(no_post(PostId))
% when the post does not exist, and throws
% commenting_disabled(PostId)) when commenting
% is disabled on the post.

bc_comment_save(PostId, Comment):-
    comment_check_answer(Comment),
    (   ds_get(PostId, [commenting], Post)
    ->  (   Post.commenting = true
        ->  comment_save(PostId, Comment)
        ;   throw(error(commenting_disabled(PostId))))
    ;   throw(error(no_post(PostId)))).

% Attaches comment timestamp,
% formats comment content and
% saves into docstore.

comment_save(PostId, Comment):-
    Content = Comment.content,
    format_comment(Content, Formatted),
    get_time(Time),
    Ts is floor(Time),
    put_dict(_{
        date: Ts,
        post: PostId,
        html: Formatted }, Comment, Processed),
    ds_insert(Processed).

% Checks the human validation
% question answer. Throws
% error(invalid_answer(Answer))) when
% answer is not correct.

comment_check_answer(Comment):-
    Id = Comment.question,
    Answer = Comment.answer,
    (   question(Id, _, Answer)
    ;   throw(error(invalid_answer(Answer)))), !.

%! bc_comment_remove(+Id) is det.
%
% Removes the given comment.

% FIXME use ds_remove/2?

bc_comment_remove(Id):-
    ds_remove(Id).

%! bc_random_question(-Id, -Question) is det.
%
% Picks random clause of question/1.

bc_random_question(Id, Question):-
    findall(question(Id, Question),
        question(Id, Question, _), Questions),
    random_member(question(Id, Question), Questions).

question(1, 'What is 2-nd digit in 03456', '3').
question(2, 'What is 3-rd digit in 03456', '4').
question(3, 'What is 4-th digit in 03456', '5').
question(4, 'Earth, Mercury and Venus are ...', 'planets').
question(5, 'Is water wet (yes/no)?', 'yes').

% Formats the comment message by
% running it through span-level Markdown
% formatter.

format_comment(Message, Formatted):-
    md_span_string(Message, Blocks),
    bc_walk(add_nofollow, Blocks, Processed),
    phrase(html(Processed), Tokens),
    with_output_to(string(Formatted), print_html(Tokens)).

% Adds nofollow to links.

add_nofollow(a(Attrs, Content), a([rel=nofollow|Attrs], Content)).
