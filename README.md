# blog-core

A Prolog library extracted from the [blog](https://github.com/rla) application.
It is work-in-progress and every piece of its API could change any time.

## Dependencies

TODO

## Configuration

 * config_get(+Name, -Value)
 * config_set(+Name, +Value)
 
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
 
## Provided functionality

TODO

### bc_html

`embed_block(+Slug)//`

Emits HTML for static block. Block contents is added without escaping. Does nothing
when the block is not found.

`embed_post(+Slug)//`

Emits HTML for the given post. Post settings (type, commenting, published)
do not matter. Does nothing when the post is not found.

`meta_headers//`

Emits HTML `<meta>` tags with configured values. When no tags have been configured,
it does nothing.

## Response mini how-to

Send HTML from `html//1`:

    :- use_module(library(http/html_write)).

    pred:-
        ...
        phrase(rule, Html),
        format('Content-type: text/html; charset=UTF-8~n~n'),
        print_html(Html).

Access the current request in the request handler:

    :- use_module(library(http/http_wrapper)).
    
    pred:-
        ...
        http_current_request(Request).

Send file as reply:

    :- use_module(library(http/http_wrapper)).
    :- use_module(library(http/http_dispatch)).

    pred:-
        ...
        http_reply_file(Path, [], Request).

Path must be relative and not contain `..`.

## Known issues

Loading `library(http/http_files)` will install some `http_dispatch` handlers
that cause error 500 response for urls like `/css/non-existent.css`.

## Profiling

Import `bc_init`:

    use_module(bc_init).
    
Start profiling server:

    bc_profile([file('blog.docstore'), port(8008)]).

This will start profiling in the HTTP worker thread. It will ask you
to hit Enter when you want to see results. Generate some traffic on the server
to have some profiling results. Load testing tool like `ab` can be used. After
you have generated enough load, press Enter. This will show profiling results.
Explanation of results is [here](http://www.swi-prolog.org/pldoc/man?section=profile).

## Running as a daemon

This uses the [http_unix_daemon](http://www.swi-prolog.org/pldoc/man?section=httpunixdaemon)
module under the hood. Example code for the main file would be:

    :- use_module(library(bc/bc_init)).
    
    bc_init:database('blog.docstore').
    
    :- use_module(library(bc/bc_daemon)).
    :- use_module(routes).
    
    :- bc_init_daemon.

Then starting as daemon takes the following command:

    swipl -s blog.pl -- --port=8001 --output=/var/log/blog.txt --pidfile=/var/run/blog.pid
