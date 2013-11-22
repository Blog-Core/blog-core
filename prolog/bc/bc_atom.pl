:- module(bc_atom, [
    send_atom/2
]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_wrapper)).

:- use_module(bc_doc).
:- use_module(bc_config).

%% reply_atom(+Title, +Posts) is det.
%
% Sends list of posts as Atom feed.
% List of posts should be ordered
% by date descending.

send_atom(Title, Posts):-
    config_get(author, Author),
    config_get(site, Site),
    feed_location(Site, Url),
    feed_update_date(Posts, Date),        
    maplist(post_to_xml(Site), Posts, EntriesXml),
    feed_xml(Title, Url, Date, Author, EntriesXml, Xml),
    send_xml(Xml).
    
feed_xml(Title, Url, Date, Author, EntriesXml, Xml):-
    XmlNs = [xmlns='http://www.w3.org/2005/Atom'],
    Xml = element(feed, XmlNs, FeedXml),
    HeadXml = [
        element(title, [], [Title]),
        element(id, [], [Url]),
        element(link, [rel=self, href=Url], []),
        element(updated, [], [Updated]),
        element(author, [], [element(name, [], [Author])])
    ],
    append(HeadXml, EntriesXml, FeedXml),
    format_time(atom(Updated), '%FT%T%:z', Date).

%% post_to_xml(+Site, +Post, -Xml) is det.
%
% Converts post to ATOM XML entry.
    
post_to_xml(Site, Post, Xml):-
    doc_get(title, Post, Title),
    doc_get(date, Post, Date),
    doc_get(slug, Post, Slug), 
    doc_get(html, Post, Content),
    atomic_list_concat([Site, '/post/show/', Slug], Url),
    entry_xml(Title, Url, Date, Content, Xml).
    
entry_xml(Title, Url, Date, Content, Xml):-
    Xml = element(entry, [], [
        element(id, [], [Url]),
        element(title, [], [Title]),
        element(updated, [], [Updated]),
        element(link, [rel=alternate, href=Url], []),
        element(content, [type=html], [Content])
    ]),
    format_time(atom(Updated), '%FT%T%:z', Date).

send_xml(Xml):-
    format('Content-type: application/atom+xml; charset=UTF-8~n~n'),
    current_output(Out),
    xml_write(Out, Xml, []).
    
feed_location(Site, Url):-    
    http_current_request(Request),
    memberchk(path(Path), Request),
    atom_concat(Site, Path, Url).

feed_update_date(Posts, Date):-
    (   Posts = []
    ->  get_time(Date)
    ;   Posts = [Post|_],
        doc_get(date, Post, Date)).
