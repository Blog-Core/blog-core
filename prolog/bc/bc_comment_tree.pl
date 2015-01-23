:- module(bc_comment_tree, [
    bc_build_comment_tree/2 % +Comments, -Tree
]).

/** <module> Builds comment tree from reply-to structure */

%! bc_build_comment_tree(+Comments, -Tree) is det.
%
% Builds comments tree from the flat list
% of comments. Sublist of comments appears
% as the replies key.

bc_build_comment_tree(Comments, Tree):-
    comments_filter(Comments, none, Top),
    build_comment_tree(Top, Comments, Tree).

build_comment_tree([Comment|Top], Comments, [Out|Tree]):-
    Comment.'$id' = TopId,
    comments_filter(Comments, option(TopId), Filtered),
    build_comment_tree(Filtered, Comments, Replies),
    put_dict(replies, Comment, Replies, Out),
    build_comment_tree(Top, Comments, Tree).

build_comment_tree([], _, []).

comments_filter([Comment|Comments], ReplyToOption, Filtered):-
    (   ReplyToOption = none
    ->  (   get_dict(reply_to, Comment, _)
        ->  Filtered = Rest
        ;   Filtered = [Comment|Rest])
    ;   ReplyToOption = option(CommentId),
        (   get_dict(reply_to, Comment, CommentId)
        ->  Filtered = [Comment|Rest]
        ;   Filtered = Rest)
    ),
    comments_filter(Comments, ReplyToOption, Rest).

comments_filter([], _, []).
