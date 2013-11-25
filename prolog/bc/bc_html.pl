:- module(bc_html, [
    embed_block//1, % +Slug,
    meta_headers//0
]).

:- use_module(library(http/html_write)).
:- use_module(library(docstore)).
:- use_module(library(error)).

:- use_module(bc_doc).

%% embed_block(+Slug)// is det.
%
% Emits HTML for static block. Block
% contents is added without escaping.

embed_block(Slug, In, Out):-
    must_be(atom, Slug),
    (   ds_find(blocks, slug=Slug, [content], [Block])
    ->  doc_get(content, Block, Content),
        html(\[Content], In, Out)
    ;   In = Out).

%% meta_headers is det.
%
% Emits HTML <meta> tags with configured values.
% When no tags have been configured, it does nothing.
    
meta_headers -->
    { ds_all(meta_headers, Headers) },
    meta_headers(Headers).
    
meta_headers([]) --> [].

meta_headers([Header|Headers]) -->
    {
        doc_get(name, Header, Name),
        doc_get(value, Header, Value)
    },
    html(meta([name=Name, content=Value])),
    meta_headers(Headers).
