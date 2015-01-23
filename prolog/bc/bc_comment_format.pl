:- module(bc_comment_format, [
    bc_format_comment/2 % -QuestionId, -Question
]).

/** <module> Handles comment formatting */

:- use_module(library(http/html_write)).
:- use_module(library(md/md_span)).

:- use_module(bc_walk).

%! bc_format_comment(+Message, -Formatted) is det.
%
% Formats the comment message by
% running it through the span-level Markdown
% formatter.

bc_format_comment(Message, Formatted):-
    md_span_string(Message, Blocks),
    bc_walk(add_nofollow, Blocks, Processed),
    phrase(html(Processed), Tokens),
    with_output_to(string(Formatted), print_html(Tokens)).

% Adds nofollow to links.

add_nofollow(a(Attrs, Content), a([rel=nofollow|Attrs], Content)).
