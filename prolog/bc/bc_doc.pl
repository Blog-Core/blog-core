:- module(bc_doc, [
    dict_sort/4    % +Name, +Dir, +List, -Sorted
]).

%% doc_sort(+Name, +Direction, +List, -Sorted) is det.
%
% Sorts the list of documents in the given direction.
% Direction is either asc or desc. Values are compared
% using the compare/3 predicate.
%
% Throws error(invalid_direction(Direction)) when
% the Direction is neither asc or desc.
%
% Throws error(no_property(Name)) when some of the
% documents in the list does not have the given
% property.
% FIXME rename to dict_sort.
    
dict_sort(Name, Direction, List, Sorted):-
    (   Direction = asc
    ->  predsort(asc(Name), List, Sorted)
    ;   (   Direction = desc
        ->  predsort(desc(Name), List, Sorted)
        ;   throw(error(invalid_direction(Direction))))).

asc(Name, Result, Doc1, Doc2):-
    get_dict_ex(Name, Doc1, Value1),
    get_dict_ex(Name, Doc2, Value2),
    compare(Result, Value1, Value2).
    
desc(Name, Result, Doc1, Doc2):-
    get_dict_ex(Name, Doc1, Value1),
    get_dict_ex(Name, Doc2, Value2),
    compare(Result, Value2, Value1).
