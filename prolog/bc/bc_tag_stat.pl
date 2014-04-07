:- module(bc_tags_stat, [
    bc_tags_stat/1 % -Dict
]).

:- use_module(library(docstore)).

%! bc_tags_stat(-Dict) is det.
%
% Finds tags that are used by published
% posts. Gives dict that has tags as keys.

bc_tags_stat(Tags):-
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
