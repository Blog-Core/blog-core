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
:- use_module(bc_string).
:- use_module(bc_type).

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
    ds_col_get(entry, Comment.post, Entry),
    notify_parent(Comment, Entry),
    notify_mentions(Comment, Entry),
    notify_entry_author(Comment, Entry).

% Notifies parent comment author.

notify_parent(Comment, Entry):-
    (   get_dict(reply_to, Comment, ParentId),
        ds_col_get(comment, ParentId, Parent),
        get_dict(notify, Parent, true),
        get_dict(email, Parent, Email),
        entry_canonical_url(Entry, URL)
    ->  Data = _{
            comment: Comment,
            parent: Parent,
            entry: Entry,
            entry_url: URL },
        bc_mail_render_template(reply, Data, Result),
        bc_config_get(smtp_from, From),
        bc_mail_enqueue_text(Result.body, From,
            Result.subject, Email)
    ;   true).

% Notify the entry author.

notify_entry_author(Comment, Entry):-
    ds_col_get(user, Entry.author, Author),
    (   Author.comment_notifications = true,
        entry_canonical_url(Entry, URL)
    ->  Data = _{
            entry: Entry,
            author: Author,
            comment: Comment,
            entry_url: URL },
        bc_mail_render_template(comment, Data, Result),
        bc_config_get(smtp_from, From),
        bc_mail_enqueue_text(Result.body, From,
            Result.subject, Author.username)
    ;   true).

% Notify users that are mentioned in the comment.

notify_mentions(Comment, Entry):-
    (   entry_canonical_url(Entry, URL)
    ->  entry_mentionable_authors(Comment.post, Authors),
        assoc_to_keys(Authors, Names),
        include(accepts_notify(Authors), Names, Accepting),
        bc_mentions_parse(Comment.content, Accepting, Mentions),
        maplist(extract_receiver(Authors), Mentions, Receivers),
        maplist(mention_send(Entry, URL, Comment), Receivers)
    ;   true).

extract_receiver(Authors, Name, Receiver):-
    get_assoc(Name, Authors, Receiver).

% Sends mention notification to the commenter
% that was mentioned.

mention_send(Entry, URL, Comment, Mention):-
    debug(bc_comment,
        'sending mention notification to ~w', [Mention.email]),
    Data = _{
        entry: Entry,
        receiver: Mention,
        comment: Comment,
        entry_url: URL },
    bc_mail_render_template(mention, Data, Result),
    bc_config_get(smtp_from, From),
    bc_mail_enqueue_text(Result.body, From,
        Result.subject, Mention.email).

accepts_notify(Authors, Name):-
    get_assoc(Name, Authors, Data),
    Data.notify = true.

% FIXME move into bc_data_mail.

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
            comment_id: Comment.'$id',
            name: Comment.author,
            notify: Comment.notify,
            email: Comment.email
        },
        put_assoc(Comment.author, Acc, New, Tmp),
        merge_author_data(Comments, Tmp, Assoc)
    ;   merge_author_data(Comments, Acc, Assoc)).

merge_author_data([], Assoc, Assoc).

% Constructs canonical URL for
% the entry. Fails when canonical
% URL pattern is not configured.

entry_canonical_url(Entry, URL):-
    bc_type_canonical(Entry.type, Canonical),
    bc_config_get(site, Site),
    bc_string_replace(Canonical, '<slug>', Entry.slug, Path),
    atom_concat(Site, Path, URL).
