:- module(bc_html, [
    embed_block//1 % +Slug
]).

:- use_module(library(http/html_write)).
:- use_module(library(docstore)).
:- use_module(library(error)).

:- use_module(bc_doc).

embed_block(Slug, In, Out):-
    must_be(atom, Slug),
    (   ds_find(blocks, slug=Slug, [content], [Block])
    ->  doc_get(content, Block, Content),
        html(\[Content], In, Out)
    ;   In = Out).
