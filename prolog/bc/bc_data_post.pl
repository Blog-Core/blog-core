:- module(bc_data_post, [
    bc_post_save/2,         % +Post, -Id
    bc_post_update/2,       % +Id, +Post
    bc_post_remove/1,       % +Id
    bc_post_find_by_slug/2, % +Slug, -Post
    bc_post_list/1,         % -List
    bc_post_list/2,         % +Type, -List
    bc_post/2,              % +Id, -Post
    bc_post_last/2,         % +Count, -List
    bc_post_tags_stat/1    % -Dict
]).

:- use_module(library(debug)).
:- use_module(library(sort_dict)).
:- use_module(library(list_util)).
:- use_module(library(docstore)).
:- use_module(library(md/md_parse)).

:- use_module(bc_data_cur_user).

bc_post_save(Post, Id):-
    get_dict_ex(slug, Post, Slug),
    ds_find(post, slug=Slug, Existing),
    length(Existing, Length),
    (   Length > 0
    ->  throw(error(existing_slug(Slug)))
    ;   bc_post_format(Post, Formatted),
        bc_user(User),
        get_dict_ex('$id', User, UserId),
        put_dict(author, Formatted, UserId, Processed),
        ds_insert(Processed, Id),
        debug(bc_data, 'saved post ~p', [Id])).

%! bc_post_update(+Id, +Post) is det.
%
% Updates the given post. Reformats HTML.

bc_post_update(Id, Post):-
    bc_post_format(Post, Formatted),
    put_dict('$id', Formatted, Id, Update),
    ds_update(Update),
    debug(bc_data, 'updated post ~p', [Id]).

% Formats post HTML contents based on
% the post's content type.

bc_post_format(PostIn, PostOut):-
    get_dict_ex(content, PostIn, Content),
    get_dict_ex(content_type, PostIn, ContentType),
    (   ContentType = markdown
    ->  md_html_string(Content, Html)
    ;   Html = Content),
    put_dict(_{ html: Html }, PostIn, PostOut).

%! bc_post_remove(+Id) is det.
%
% Removes the given post and its comments.

bc_post_remove(Id):-
    ds_remove(Id),
    ds_remove(comment, post=Id),
    debug(bc_data, 'removed post ~p', [Id]).

%! bc_post_find_by_slug(+Slug, -Post) is semidet.
%
% Finds post. Fails when no post is found.

bc_post_find_by_slug(Slug, Post):-
    ds_find(post, slug=Slug, [Post]).

%! bc_post_list(-List) is det.
%
% Retrieves the list of all posts.
% Does not include contents and HTML.
% FIXME add comment count.

bc_post_list(List):-
    ds_all(post, [slug, type, date_published,
        date_updated, commenting, published,
        title, author], List).

%! bc_post_list(+Type, -List) is det.
%
% Retrieves the list of posts of certain type.
% Does not include contents and HTML.
% FIXME add comment count.

bc_post_list(Type, List):-
    ds_find(post, type=Type, [slug, type, date_published,
        date_updated, commenting, published,
        title, author], List).

%! bc_post(+Id, -Post) is det.
%
% Retrieves a single post by ID. Throws
% error(no_post(Id)) when there is no such post.

bc_post(Id, Post):-
    (   ds_get(Id, [slug, type, date_published, date_updated,
            commenting, published, title, author,
            content, description, content_type, tags], Post)
    ;   throw(error(no_post(Id)))), !.

%! bc_post_last(+Count, -Posts) is det.
%
% Retrieves last posts (by publish date).
% TODO optimize?

bc_post_last(Count, Posts):-
    ds_find(post, (published=true, type=post), All),
    sort_dict(date, desc, All, Sorted),
    take(Sorted, Count, Posts).

%! bc_post_tags_stat(-Dict) is det.
%
% Finds tags that are used by published
% posts. Gives dict that has tags as keys.

bc_post_tags_stat(Tags):-
    ds_find(post, (published=true, type=post), [tags], Posts),
    tags_statistics(Posts, _{}, Tags).

tags_statistics([Post|Posts], Acc, Tags):-
    get_dict_ex(tags, Post, PostTags),
    update_tagstat(PostTags, Acc, Tmp),
    tags_statistics(Posts, Tmp, Tags).

tags_statistics([], Acc, Sorted):-
    tag_stat_dicts(Acc, Tags),
    sort_dict(count, desc, Tags, Sorted).

update_tagstat([Tag|Tags], In, Out):-
    (   get_dict(Tag, In, Old)
    ->  New is Old + 1,
        put_dict(Tag, In, New, Tmp)
    ;   put_dict(Tag, In, 1, Tmp)),
    update_tagstat(Tags, Tmp, Out).

update_tagstat([], Acc, Acc).

% Turns stat dict into list of
% smaller dicts _{ tag: Tag, count: Count }.

tag_stat_dicts(Stats, List):-
    dict_pairs(Stats, _, Pairs),
    tag_stat_dicts_list(Pairs, List).

tag_stat_dicts_list([Tag-Count|Pairs], [Dict|List]):-
    Dict = _{ tag: Tag, count: Count },
    tag_stat_dicts_list(Pairs, List).

tag_stat_dicts_list([], []).
