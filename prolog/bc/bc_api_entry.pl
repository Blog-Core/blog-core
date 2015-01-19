:- module(bc_api_entry, []).

/** <module> HTTP handlers for managing posts */

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_view).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_data_entry).

% Adds new entry.

:- route_post(api/entry,
    bc_auth, entry_save).

entry_save:-
    bc_read_by_schema(entry, Post),
    bc_entry_save(Post, Id),
    bc_view_purge_cache,
    bc_reply_success(Id).

% Updates the given entry.

:- route_put(api/entry/Id,
    bc_auth, entry_update(Id)).

entry_update(Id):-
    bc_read_by_schema(entry, Entry),
    bc_entry_update(Id, Entry),
    bc_view_purge_cache,
    bc_reply_success(Id).

% Removes the entry.

:- route_del(api/entry/Id,
    bc_auth, entry_remove(Id)).

entry_remove(Id):-
    bc_entry_remove(Id),
    bc_view_purge_cache,
    bc_reply_success(Id).

% List of entries of certain type.

:- route_get(api/entries/Type,
    bc_auth, entry_list(Type)).

entry_list(Type):-
    bc_entry_list(Type, List),
    bc_reply_success(List).

% Single entry with contents.

:- route_get(api/entry/Id,
    bc_auth, entry_get(Id)).

entry_get(Id):-
    bc_entry(Id, Entry),
    bc_reply_success(Entry).

% Single entry without contents.

:- route_get(api/entry/Id/info,
    bc_auth, entry_get_info(Id)).

entry_get_info(Id):-
    bc_entry_info(Id, Entry),
    bc_reply_success(Entry).

% Entry schema.
% can probably be reduced.

:- register_schema(entry, _{
    type: dict,
    tag: entry,
    keys: _{
        author: _{ type: atom, min_length: 36, max_length: 36 },
        title: _{ type: string, min_length: 1 },
        slug: _{ type: atom, min_length: 1 },
        tags: _{ type: list, items: atom },
        date_published: _{ type: integer, min: 0 },
        date_updated: _{ type: integer, min: 0 },
        commenting: bool,
        published: bool,
        content: string,
        content_type: _{ type: enum, values: [markdown, raw] },
        description: string,
        type: _{ type: atom, min_length: 1, max_length: 100 },
        language: _{ type: string, min_length: 2, max_length: 10 }
    }
}).
