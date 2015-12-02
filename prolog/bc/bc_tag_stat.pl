:- module(bc_tag_stat, [
    bc_tag_stat/1,     % -Tags
    bc_tag_stat/2,     % +Type, -Tags
    bc_tag_stat_all/2  % +Type, -Tags
]).

/** <module> Tag statistics */

:- use_module(library(assoc)).
:- use_module(library(docstore)).
:- use_module(library(sort_dict)).

%! bc_tag_stat(+Type, -Tags) is det.
%
% Finds tags that are used by published
% posts. Gives list of dicts
% _{ tag: Tag, count: Count }

bc_tag_stat(Type, Tags):-
    must_be(atom, Type),
    ds_find(entry,
        (published=true, type=Type),
        [tags], Posts),
    tag_statistics(Posts, _{}, Tags).

%! bc_tag_stat_all(+Type, -Tags) is det.
%
% Finds tags that are used by all
% posts. Otherwise same as bc_tag_stat/2.

bc_tag_stat_all(Type, Tags):-
    must_be(atom, Type),
    ds_find(entry, type=Type, [tags], Posts),
    tag_statistics(Posts, _{}, Tags).

%! bc_tag_stat(-Tags) is det.
%
% Same as bc_tag_stat/2 but uses
% post as the entry type.

bc_tag_stat(Tags):-
    bc_tag_stat(post, Tags).

tag_statistics([Post|Posts], Acc, Tags):-
    PostTags = Post.tags,
    update_tagstat(PostTags, Acc, Tmp),
    tag_statistics(Posts, Tmp, Tags).

tag_statistics([], Acc, Sorted):-
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
