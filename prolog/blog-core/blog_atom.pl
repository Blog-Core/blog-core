:- module(blog_atom, [
    send_atom/1
]).

:- use_module(library(http/html_write)).

:- use_module(blog_prop).

%% reply_atom(+Feed) is det.
%
% Sends list of posts as Atom feed.

send_atom(Feed):-
    XML = element(feed, [xmlns = 'http://www.w3.org/2005/Atom'], [
        element(title, [], [Title]),
        element(id, [], [Id]),
        element(link, [rel=self, href=Url], []),
        element(updated, [], [Updated]),
        element(author, [], [
            element(name, [], [Author])
        ])
        | Entries
    ]),
    prop_get(title, Feed, Title),
    prop_get(id, Feed, Id),
    prop_get(url, Feed, Url),
    prop_get(updated, Feed, Date),
    prop_get(author, Feed, Author),
    prop_get(posts, Feed, Posts),
    format_time(atom(Updated), '%FT%T%:z', Date),
    maplist(post_to_xml, Posts, Entries),
    format('Content-type: application/atom+xml; charset=UTF-8~n~n'),
    current_output(Out),
    xml_write(Out, XML, []).

%% post_to_xml(+Post, -XML) is det.
%
% Converts post to ATOM XML entry.
% Dates are represented using ISO-8601.
    
post_to_xml(Post, XML):-
    XML = element(entry, [], [
        element(id, [], [Url]),
        element(title, [], [Title]),
        element(updated, [], [Updated]),
        element(link, [rel=alternate, href=Url], []),
        element(content, [type=html], [Content])
    ]),
    prop_get(title, Post, Title),
    prop_get(date, Post, Date),
    prop_get(url, Post, Url),    
    prop_get(html, Post, Content),
    format_time(atom(Updated), '%FT%T%:z', Date).
