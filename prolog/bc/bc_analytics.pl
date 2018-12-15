:- module(bc_analytics, [
    bc_analytics_record_user/2,             % +Data, -UserId
    bc_analytics_record_session/2,          % +Data, -SessionId
    bc_analytics_record_pageview/2,         % +Data, -PageviewId
    bc_analytics_record_pageview_extend/1,  % +Data
    bc_enable_analytics/1,                  % +Path
    bc_analytics_flush_output/0,
    bc_month_file_name/3                    % + Year, +Month, -File
]).

/** <module> Generic visitor tracking analytics */

:- use_module(library(url)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(docstore)).

:- dynamic(enabled/0).
:- dynamic(path/1).
:- dynamic(stream/1).
:- dynamic(open_month/2).

% Stores the user data while generating the
% new random user identifier.

bc_analytics_record_user(UserData, UserId):-
    must_be(dict, UserData),
    ds_uuid(UserId),
    integer_timestamp(TimeStamp),
    record_entry(UserData.put(_{
        user_id: UserId,
        timestamp: TimeStamp})).

% Stores the session data while generating the
% new random session identifier.

bc_analytics_record_session(SessionData, SessionId):-
    must_be(dict, SessionData),
    ds_uuid(SessionId),
    integer_timestamp(TimeStamp),
    record_entry(SessionData.put(_{
        session_id: SessionId,
        timestamp: TimeStamp})).

bc_analytics_record_pageview(PageviewData, PageviewId):-
    must_be(dict, PageviewData),
    (   get_dict(pageview_id, PageviewData, PageviewId)
    ->  true
    ;   ds_uuid(PageviewId)),
    parse_url(PageviewData.location, UrlParts),
    memberchk(path(Path), UrlParts),
    integer_timestamp(TimeStamp),
    record_entry(PageviewData.put(_{
        location: Path,
        pageview_id: PageviewId,        
        timestamp: TimeStamp})).

bc_analytics_record_pageview_extend(PageviewData):-
    record_entry(PageviewData).

% Timestamp rounded as an integer. Leads
% to a more compact log files.

integer_timestamp(TimeStamp):-
    get_time(Time),
    TimeStamp is round(Time).

record_entry(Entry):-    
    with_mutex(serialized_write, record_entry_unsafe(Entry)).

record_entry_unsafe(_):-
    \+ enabled, !.

record_entry_unsafe(Entry):- 
    output_stream(Stream),
    write_canonical_term(Stream, Entry).

output_stream(Stream):-
    (   open_month(Year, Month)
    ->  (   current_month(Year, Month)
        ->  stream(Stream)
        ;   open_output_stream(Stream))
    ;   open_output_stream(Stream)).

% Opens a fresh output stream.
% This happens when a month boundary is crossed.

open_output_stream(Stream):-
    (   stream(OldStream)
    ->  close(OldStream),
        retractall(stream(_))
    ;   true),
    current_month(Year, Month),
    bc_month_file_name(Year, Month, File),
    open(File, append, Stream, [encoding('utf8')]),
    retractall(stream(Stream)),
    retractall(open_month(_, _)),
    asserta(stream(Stream)),
    asserta(open_month(Year, Month)).

bc_month_file_name(Year, Month, File):-
    path(Base),
    atomic_list_concat([Base, '/', Year, '-', Month, '.pl'], File).

% Extracts current month.

current_month(Year, Month):-
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTime, 'UTC'),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month).

% Writes given term in canonical form that makes
% it possible to read back.

write_canonical_term(Stream, Term):-
    write_term(Stream, Term, [
        ignore_ops,
        quoted,
        dotlists(true),
        nl(true),
        fullstop(true)
    ]).

% Enables analytics and sets the directory where
% to store there analytics logs.

bc_enable_analytics(_):-
    enabled, !.

bc_enable_analytics(Path):-
    debug(bc_analytics, 'Enabling analytics with storage at ~w.', [Path]),
    (   exists_directory(Path)
    ->  (   access_file(Path, write)
        ->  true
        ;   throw(error(analytics_directory_not_writable)))
    ;   throw(error(analytics_directory_not_exists))),
    asserta(enabled),
    asserta(path(Path)).

% Flushes the log stream.

bc_analytics_flush_output:-
    (   stream(Stream)
    ->  flush_output(Stream)
    ;   true).
