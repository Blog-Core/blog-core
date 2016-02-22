:- module(bc_comment_notify, [
    bc_comment_notify/1 % +CommentId
]).

:- use_module(library(assoc)).
:- use_module(library(error)).
:- use_module(library(docstore)).
:- use_module(library(sort_dict)).

:- use_module(bc_comment_mention).
:- use_module(bc_mail_template).
:- use_module(bc_data_config).
:- use_module(bc_mail_queue).

% Send notifications for the given
% comment. Fails if comment does not
% exist.

%! bc_comment_notify(CommentId) is det.
%
% Sends comment notifications. Includes
% mentions and notification to the entry
% author.

bc_comment_notify(CommentId):-
    must_be(atom, CommentId),
    debug(bc_comment,
        'sending comment ~w notifications', [CommentId]),
    ds_col_get(comment, CommentId, Comment),
    entry_mentionable_authors(Comment.post, Authors),
    ds_col_get(entry, Comment.post, Entry),
    assoc_to_keys(Authors, Names),
    include(accepts_notify(Authors), Names, Accepting),
    bc_mentions_parse(Comment.content, Accepting, Mentions),
    maplist(extract_receiver(Authors), Mentions, Receivers),
    maplist(mention_send(Entry, Comment), Receivers).

% Sends comment notification to the
% post author when SMTP is enabled
% and user wants to receive notifications.

/*
comment_notify(EntryId, Comment):-
    bc_entry_author(EntryId, AuthorId),
    ds_col_get(user, AuthorId, Author),
    bc_entry_title(EntryId, Title),
    bc_config_get(smtp_from, From),
    atom_concat('Comment notification: ', Title, Subject),
    (   Author.comment_notifications = true
    ->  bc_mail_send(comment_notify_body(Comment),
            From, Subject, Author.username)
    ;   true).*/

% Generates body for the comment
% mail notification.
/*
comment_notify_body(Comment, Out):-
    format(Out, 'Comment author: ~w~n~n', [Comment.author]),
    writeln(Out, Comment.content).*/

extract_receiver(Authors, Name, Receiver):-
    get_assoc(Name, Authors, Receiver).

% Sends mention notification to the commenter
% that was mentioned.

mention_send(Entry, Comment, Mention):-
    debug(bc_comment,
        'sending mention notification to ~w', [Mention.email]),
    Data = _{
        entry: Entry,
        receiver: Mention,
        comment: Comment },
    bc_mail_render_template(mention, Data, Result),
    bc_config_get(smtp_from, From),
    bc_mail_enqueue_text(Result.body, From,
        Result.subject, Mention.email).

accepts_notify(Authors, Name):-
    get_assoc(Name, Authors, Data),
    Data.notify = true.

% Finds all comment authors for the
% given entry. Authors is an assoc.

entry_mentionable_authors(EntryId, Authors):-
    must_be(atom, EntryId),
    ds_find(comment, post=EntryId,
        [author, notify, email, date], Comments),
    sort_dict(date, desc, Comments, Sorted),
    merge_author_data(Sorted, Authors).

merge_author_data(Comments, Assoc):-
    empty_assoc(Empty),
    merge_author_data(Comments, Empty, Assoc).

merge_author_data([Comment|Comments], Acc, Assoc):-
    (   get_dict(email, Comment, _),
        get_dict(notify, Comment, true)
    ->  New = _{
            name: Comment.author,
            notify: Comment.notify,
            email: Comment.email
        },
        put_assoc(Comment.author, Acc, New, Tmp),
        merge_author_data(Comments, Tmp, Assoc)
    ;   merge_author_data(Comments, Acc, Assoc)).

merge_author_data([], Assoc, Assoc).
