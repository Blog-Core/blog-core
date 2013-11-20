:- module(blog_doc, [
    prop_get/3,
    prop_sort/4,
    doc_to_json/2,
    json_to_doc/2
]).

prop_get(Name, Doc, Value):-
    Term =.. [Name,Value],
    memberchk(Term, Doc), !.
    
prop_get(Name, _, _):-
    throw(error(no_property(Name))).

prop_sort(Name, asc, List, Sorted):-
    predsort(asc(Name), List, Sorted).

prop_sort(Name, desc, List, Sorted):-
    predsort(desc(Name), List, Sorted).

asc(Name, Result, Doc1, Doc2):-
    prop_get(Name, Doc1, Value1),
    prop_get(Name, Doc2, Value2),
    compare(Result, Value1, Value2).
    
desc(Name, Result, Doc1, Doc2):-
    prop_get(Name, Doc1, Value1),
    prop_get(Name, Doc2, Value2),
    compare(Result, Value2, Value1).

json_to_doc(json(Props), Doc):- !,
    maplist(json_to_prop, Props, Doc).
    
json_to_doc(JSON, Doc):-
    is_list(JSON), !,
    maplist(json_to_doc, JSON, Doc).
    
json_to_doc(@(true), true).

json_to_doc(@(false), false).

json_to_doc(@(null), null).

json_to_doc(Atom, Atom):-
    atom(Atom).

json_to_doc(Number, Number):-
    number(Number).
    
json_to_prop(Name=JSONValue, Term):-
    Term =.. [ Name, Value ],
    json_to_doc(JSONValue, Value).

doc_to_json(Doc, json(JSONProps)):-
    maplist(prop_to_json, Doc, JSONProps).
    
prop_to_json(Term, Name = JSONValue):-
    Term =.. [ Name, Value ],
    value_to_json(Value, JSONValue).
    
value_to_json(true, @(true)).    

value_to_json(false, @(false)).

value_to_json(null, @(null)).

value_to_json(Atom, Atom):-
    atom(Atom).
    
value_to_json(Number, Number):-
    number(Number).

% Value is considered a doc when
% it contains at least one property term.
    
value_to_json(Doc, JSONDoc):-
    is_doc(Doc), !,
    doc_to_json(Doc, JSONDoc).

value_to_json(List, JSONArray):-
    is_list(List),
    maplist(value_to_json, List, JSONArray).
    
value_to_json(Term, Atom):-
    term_to_atom(Term, Atom).
    
is_doc([Term|_]):-
    functor(Term, _, 1).
