:- module(bc_data_comment_question, [
    bc_random_question/2, % -QuestionId, -Question
    bc_answer_ok/2        % +QuestionId, +Answer
]).

/** <module> Handles human check for comments */

% Checks the human validation
% question answer.

bc_answer_ok(QuestionId, Answer):-
    question(QuestionId, _, Answer).

%! bc_random_question(-Id, -Question) is det.
%
% Picks random question for human testing.

bc_random_question(Id, Question):-
    findall(question(Id, Question),
        question(Id, Question, _), Questions),
    random_member(question(Id, Question), Questions).

question(1, 'What is 2-nd digit in 03456', '3').
question(2, 'What is 3-rd digit in 03456', '4').
question(3, 'What is 4-th digit in 03456', '5').
question(4, 'Earth, Mercury and Venus are ...', 'planets').
question(5, 'Is water wet (yes/no)?', 'yes').
