# blog-core

A Prolog library extracted from the [blog](https://github.com/rla) application.
It is work-in-progress and every piece of its API could change any time.

## Features

 * Document-based in-memory data storage.
 * Generic administration interface.
 * Simple HTTP request routing.
 * Built-in cache busting.

## Dependencies

TODO
 
## Modules

### bc_doc

Provides various predicates for working with documents.

 * `doc_id(+Doc, -Id)`. Extracts ID from the given document.
 
## Admin interface

For a document collection to appear in the admin interface, it must be first
registered into types collection. The document describing display/edit
information has to be inserted.

TODO

This creates descriptor document for documents in the `config` collection.
Meaning of terms is the following:

 * `name(config)` - the name of the collection.
 * `list([ name, value ])` - display in the list view properties `name` and `value`.
    
### Property types

 * `line` - single-line text. Editor `<input type="text">` is used.
 * `multiline` - multi-line text. Editor `<textarea></textarea>` is used.
 * `password` - password with masked editor. Editor `<input type="password">` is used.
 * `boolean` - checkbox. Editor `<input type="checkbox">` is used.
 * `choice` - combobox. Editor `<select></select>` is used.

### bc_bust

Loading this module will install rewrite handler to remove
the request URL path prefix `/t-[0-9]+`.

`bs_bust_token(-Token)`

Retrieves the token for cache busting. Currently it is set to
the server start timestamp.

When serving static files with Nginx, the rewrite rule can be
handled with Nginx's own rewrite rule:

    location ~ ^/t-\d+/(.*)$ {
        access_log off;
        rewrite ^/t-\d+/(.*)$ /$1 last;
    }

## Running with REPL

    swipl -s main.pl -- --port=8001 --fork=false

## Profiling single HTTP thread

Start with a single HTTP worker thread:

    swipl -- --port=18008 --fork=false --workers=1

Import thread_httpd:

    ?- use_module(library(http/thread_httpd)).

Show HTTP thread ids:

    ?- http_current_worker(8001, ThreadId).

Enable profiling on a thread:

    ?- tprofile(ThreadId).

## Running as a daemon

This uses the [http_unix_daemon](http://www.swi-prolog.org/pldoc/man?section=httpunixdaemon)
module under the hood.

Then starting as daemon takes the following command:

    swipl -s main.pl -- --port=8001

## Running tests

    make tests
