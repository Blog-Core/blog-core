:- module(bc_doc, [
    doc_id/2,      % +Doc, -Id
    doc_get/3,     % +Name, +Doc, -Value
    doc_replace/4, % +Name, +Value, +DocIn, -DocOut
    doc_sort/4,    % +Name, +Dir, +List, -Sorted
    doc_to_json/2, % +Json, -Doc
    json_to_doc/2  % +Doc, -Json,    
]).



%% doc_id(+Doc, -Id) is det.
%
% Extracts the document id.
% Throws error(document_has_no_id))
% when the document does not contain id.

doc_id(Doc, Id):-
    (   memberchk('$id'(Id), Doc)
    ->  true
    ;   throw(error(document_has_no_id))).

%% doc_get(+Name, +Doc, -Value) is det.
%
% Extracts the given property value
% from the document.
% Throws error(no_property(Name)) when
% the document has no given property.
    
doc_get(Name, Doc, Value):-
    Term =.. [Name, Value],
    (   memberchk(Term, Doc)
    ->  true
    ;   throw(error(no_property(Name)))).

%% doc_replace(+Name, +Value, +In, -Out) is det.
%
% Replaces (or adds) the property value
% in the given document.
    
doc_replace(Name, Value, In, Out):-
    Old =.. [Name, _],
    New =.. [Name, Value],
    (   select(Old, In, Tmp)
    ->  Out = [New|Tmp]
    ;   Out = [New|In]).

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
    
doc_sort(Name, Direction, List, Sorted):-
    (   Direction = asc
    ->  predsort(asc(Name), List, Sorted)
    ;   (   Direction = desc
        ->  predsort(desc(Name), List, Sorted)
        ;   throw(error(invalid_direction(Direction))))).

asc(Name, Result, Doc1, Doc2):-
    doc_get(Name, Doc1, Value1),
    doc_get(Name, Doc2, Value2),
    compare(Result, Value1, Value2).
    
desc(Name, Result, Doc1, Doc2):-
    doc_get(Name, Doc1, Value1),
    doc_get(Name, Doc2, Value2),
    compare(Result, Value2, Value1).

%% json_to_doc(+Json, -Doc) is det.
%
% Converts the given JSON document into
% a normal document.
%
% JSON documents are expected to be in
% the format described in
% http://www.swi-prolog.org/pldoc/man?section=json
%
% Throws error(json_cannot_convert(Json)) when
% the input term cannot be converted.

json_to_doc(json(Props), Doc):-
    maplist(json_to_prop, Props, Doc).

json_to_doc(@(true), true).

json_to_doc(@(false), false).

json_to_doc(@(null), null).

json_to_doc(Atom, Atom):-
    atom(Atom).

json_to_doc(Number, Number):-
    number(Number).
    
json_to_doc(Json, Doc):-
    is_list(Json),
    maplist(json_to_doc, Json, Doc).
    
json_to_doc(Json, _):-
    throw(error(json_cant_convert(Json))).
    
json_to_prop(Name=Json, Term):-
    Term =.. [Name, Value],
    json_to_doc(Json, Value).

%% doc_to_json(+Doc, -Json) is det.
%
% Converts normal document into a
% JSON document.
    
doc_to_json(Doc, json(Props)):-
    maplist(prop_to_json, Doc, Props).
    
prop_to_json(Term, Name=Json):-
    Term =.. [Name, Value],
    value_to_json(Value, Json).

value_to_json(true, @(true)).    

value_to_json(false, @(false)).

value_to_json(null, @(null)).

value_to_json(Atom, Atom):-
    atom(Atom).
    
value_to_json(Number, Number):-
    number(Number).
    
value_to_json(Doc, Json):-
    is_doc(Doc),
    doc_to_json(Doc, Json).

value_to_json(List, Json):-
    is_list(List),
    maplist(value_to_json, List, Json).
    
value_to_json(Term, Atom):-
    term_to_atom(Term, Atom).
    
is_doc([Term|_]):-
    functor(Term, _, 1).
