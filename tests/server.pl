:- asserta(user:file_search_path(library, '/home/raivo/docstore/prolog')).
:- asserta(user:file_search_path(library, '/home/raivo/alternative-router/prolog')).
:- asserta(user:file_search_path(library, '/home/raivo/prolog-markdown/prolog')).

:- use_module(library(debug)).
:- use_module(library(docstore)).
:- use_module(prolog/bc/bc_daemon).

:- debug(bc_daemon).
:- debug(bc_data).
:- debug(bc_config).
:- debug(bc_data).
:- debug(docstore).

:- ds_open('site.docstore').
:- bc_daemon.
