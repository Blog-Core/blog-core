:- module(bc_analytics_read, [
    bc_analytics_read/3 % +From, +To, -Module
]).

/** <module> Generic visitor tracking analytics */

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(gensym)).
:- use_module(bc_analytics).

% Reads analytics data into the given module.
% From and To are terms in the form: (Year, Month).

bc_analytics_read(From, To, Module):-
    must_be(ground, From),
    must_be(ground, To),
    gensym(analytics_cache_module_, Module),
    dynamic(Module:user/1),
    dynamic(Module:session/1),
    dynamic(Module:pageview/1),
    findall(Name, file_name(From, To, Name), Names),
    maplist(read_file_into(Module), Names),
    compute_session_durations(Module),
    compute_user_durations(Module).

read_file_into(Module, File):-
    debug(bc_analytics, 'Reading file ~w into module ~w.', [File, Module]),
    setup_call_cleanup(
        open(File, read, Stream, [encoding('utf8')]),
        read_stream_into(Module, Stream),
        close(Stream)).

% Reads and loads the terms from the given
% file into the target module.

read_stream_into(Module, Stream):-
    catch(
        read_term(Stream, Term, [dotlists(true)]),
        E, true),
    (   var(E)
    ->  (   Term = end_of_file
        ->  true
        ;   load_term_into(Module, Term),
            read_stream_into(Module, Stream))
    ;   true).

load_term_into(Module, Term):-
    is_dict(Term, Tag),
    load_dict_term_into(Tag, Module, Term).

% TODO: add session count, page count

load_dict_term_into(user, Module, Dict):- !,
    UserId = Dict.user_id,
    assertz(Module:user(UserId)),
    assertz(Module:user_duration(UserId, 0)),
    assertz(Module:user_timestamp(UserId, Dict.timestamp)).

load_dict_term_into(session, Module, Dict):-
    UserId = Dict.user_id,
    call(Module:user(UserId)), !,
    SessionId = Dict.session_id,
    assertz(Module:session(SessionId)),
    assertz(Module:session_user(SessionId, UserId)),
    assertz(Module:session_duration(SessionId, 0)),
    assertz(Module:session_pagecount(SessionId, 0)),
    assertz(Module:session_timestamp(SessionId, Dict.timestamp)),
    assertz(Module:session_agent(SessionId, Dict.agent)),
    assertz(Module:session_platform(SessionId, Dict.platform)).

% TODO: add title and entry id.
load_dict_term_into(pageview, Module, Dict):-
    SessionId = Dict.session_id,
    call(Module:session(SessionId)), !,
    PageviewId = Dict.pageview_id,
    assertz(Module:pageview(PageviewId)),
    assertz(Module:pageview_session(PageviewId, SessionId)),
    assertz(Module:pageview_duration(PageviewId, 0)),
    assertz(Module:pageview_timestamp(PageviewId, Dict.timestamp)),
    assertz(Module:pageview_location(PageviewId, Dict.location)),
    assertz(Module:pageview_referrer(PageviewId, Dict.referrer)),
    call(Module:session_pagecount(SessionId, PageCount)),
    retractall(Module:session_pagecount(SessionId, _)),
    NewPageCount is PageCount + 1,
    assertz(Module:session_pagecount(SessionId, NewPageCount)).

load_dict_term_into(pageview_extend, Module, Dict):-
    PageviewId = Dict.pageview_id,
    call(Module:pageview(PageviewId)), !,
    retractall(Module:pageview_duration(PageviewId, _)),
    assertz(Module:pageview_duration(PageviewId, Dict.elapsed)).

load_dict_term_into(_, _, _).

% Computes total session durations from pageview
% durations.

compute_session_durations(Module):-
    findall(SessionId, call(Module:session(SessionId)), Sessions),
    maplist(compute_session_duration(Module), Sessions).

compute_session_duration(Module, SessionId):-
    findall(Duration, (
        call(Module:pageview_session(PageviewId, SessionId)),
        call(Module:pageview_duration(PageviewId, Duration))), Durations),
    sum_list(Durations, Total),
    retractall(Module:session_duration(SessionId, _)),
    assertz(Module:session_duration(SessionId, Total)).

% Computes total user durations from session durations.

compute_user_durations(Module):-
    findall(UserId, call(Module:user(UserId)), Users),
    maplist(compute_user_duration(Module), Users).

compute_user_duration(Module, UserId):-
    findall(Duration, (
        call(Module:session_user(SessionId, UserId)),
        call(Module:session_duration(SessionId, Duration))), Durations),
    sum_list(Durations, Total),
    retractall(Module:user_duration(UserId, _)),
    assertz(Module:user_duration(UserId, Total)).

file_name(From, To, File):-
    From = (YearFrom, MonthFrom),
    To = (YearTo, MonthTo),
    between(YearFrom, YearTo, Year),
    between(1, 12, Month),
    (   Year = YearFrom
    ->  Month >= MonthFrom
    ;   true),
    (   Year = YearTo
    ->  Month =< MonthTo
    ;   true),
    bc_month_file_name(Year, Month, File),
    exists_file(File).
