# blog-core

A CMS/blog framework for [Swi-Prolog](http://swi-prolog.org/). It is currently work-in-progress.

## Features

 * Multiple users.
 * Multilanguage support.
 * Markdown for content.
 * Mail notifications.
 * Simple security model.
 * Simple HTTP request routing.
 * Document-based in-memory data storage.
 * Threaded comments.
 * Built-in view caching.
 * Built-in client-side cache busting.
 * Administration API.
 * Administration UI that supports mobile devices.
 * Data migrations.

## Current issues

Swi-Prolog does not contain mechanism to set process-wide UTF-8 encoding
for source files and therefore you need to use `:- encoding(utf8).` at the
beginning of each source file where you want to use UTF-8.

## License

The MIT license. See the LICENSE file.
