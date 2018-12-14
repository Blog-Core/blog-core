:- module(bc_analytics_db, [
    bc_analytics_user_ts/3,      % +Interval, +MinDuration, -Series
    bc_analytics_session_ts/3,   % +Interval, +MinDuration, -Series
    bc_analytics_pageview_ts/3,  % +Interval, +MinDuration, -Series
    bc_analytics_users/5,    % +Interval, +MinDuration, +Offset, +Count, -Users
    bc_analytics_sessions/5  % +Interval, +MinDuration, +Offset, +Count, -Sessions
]).

/** <module> Generic visitor tracking analytics */

:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(bc_analytics_read).
:- use_module(bc_analytics_ts).

:- dynamic(analytics_cache/3).

% Validates the analytics interval.
% This is used for loading the analytics
% database and cache the loaded data.

valid_interval(Interval):-
    Interval = (YearFrom, MonthFrom)-(YearTo, MonthTo),
    YearFrom >= 2018, YearFrom =< 2100,
    YearTo >= 2018, YearTo =< 2100,
    MonthFrom >= 1, MonthFrom =< 12,
    MonthTo >= 1, MonthTo =< 12, !.

valid_interval(Interval):-
    throw(error(invalid_analytics_interval(Interval))).

% Loads the analytics database into a module
% or uses the previously loaded database.

analytics_module(Interval, Module):-
    with_mutex(analytics_cache,
        analytics_module_unsafe(Interval, Module)).

analytics_module_unsafe(Interval, Module):-
    must_be(ground, Interval),
    valid_interval(Interval),
    (   analytics_cache(Interval, Module, _)
    ->  debug(bc_analytics,
            'Using cached analytics for ~w.', [Interval])
    ;   Interval = From-To,
        bc_analytics_read(From, To, Module),
        get_time(TimeStamp),
        assertz(analytics_cache(Interval, Module, TimeStamp))).

% Timeseries analytics.

% Calculates the daily time series of the
% new user count.

bc_analytics_user_ts(Interval, MinDuration, SeriesAsList):-
    analytics_module(Interval, Module),
    bc_analytics_ts_zero(Interval, ZeroSeries),
    findall(UserId, (
        call(Module:user(UserId)),
        call(Module:user_duration(UserId, Duration)),
        Duration >= MinDuration), AllUserIds),
    fill_user_ts(AllUserIds, Module, ZeroSeries, Series),
    bc_analytics_ts_list(Series, SeriesAsList).

fill_user_ts([UserId|UserIds], Module, SeriesIn, SeriesOut):-
    call(Module:user_timestamp(UserId, TimeStamp)),
    bc_analytics_ts_incr(SeriesIn, TimeStamp, SeriesTmp),
    fill_user_ts(UserIds, Module, SeriesTmp, SeriesOut).

fill_user_ts([], _, Series, Series).

% Calculates the daily time series of the
% new session count.

bc_analytics_session_ts(Interval, MinDuration, SeriesAsList):-
    analytics_module(Interval, Module),
    bc_analytics_ts_zero(Interval, ZeroSeries),
    findall(SessionId, (
        call(Module:session(SessionId)),
        call(Module:session_duration(SessionId, Duration)),
        Duration >= MinDuration), AllSessionIds),
    fill_session_ts(AllSessionIds, Module, ZeroSeries, Series),
    bc_analytics_ts_list(Series, SeriesAsList).

fill_session_ts([SessionId|SessionIds], Module, SeriesIn, SeriesOut):-
    call(Module:session_timestamp(SessionId, TimeStamp)),
    bc_analytics_ts_incr(SeriesIn, TimeStamp, SeriesTmp),
    fill_session_ts(SessionIds, Module, SeriesTmp, SeriesOut).

fill_session_ts([], _, Series, Series).

% Calculates the daily time series of the
% pageview count. Only those pageviews from the
% sessions passing the minimum duration are considered.

bc_analytics_pageview_ts(Interval, MinDuration, SeriesAsList):-
    analytics_module(Interval, Module),
    bc_analytics_ts_zero(Interval, ZeroSeries),
    findall(PageviewId, (
        call(Module:pageview(PageviewId)),
        call(Module:pageview_session(PageviewId, SessionId)),
        call(Module:session(SessionId)),
        call(Module:session_duration(SessionId, Duration)),
        Duration >= MinDuration), AllPageviewIds),
    fill_pageview_ts(AllPageviewIds, Module, ZeroSeries, Series),
    bc_analytics_ts_list(Series, SeriesAsList).

fill_pageview_ts([PageviewId|PageviewIds], Module, SeriesIn, SeriesOut):-
    call(Module:pageview_timestamp(PageviewId, TimeStamp)),
    bc_analytics_ts_incr(SeriesIn, TimeStamp, SeriesTmp),
    fill_pageview_ts(PageviewIds, Module, SeriesTmp, SeriesOut).

fill_pageview_ts([], _, Series, Series).

% List of users that have spent more time on the site
% than the given minimum duration.

bc_analytics_users(Interval, MinDuration, Offset, Count, Users):-
    analytics_module(Interval, Module),
    findall(UserId, (
        call(Module:user(UserId)),
        call(Module:user_duration(UserId, Duration)),
        Duration >= MinDuration), AllUserIds),
    sublist_offset_count(AllUserIds, Offset, Count, UserIds),
    maplist(user_data(Module), UserIds, Users).

% List of sessions that are longer
% than the given minimum duration.

bc_analytics_sessions(Interval, MinDuration, Offset, Count, Sessions):-
    analytics_module(Interval, Module),
    findall(SessionId, (
        call(Module:session(SessionId)),
        call(Module:session_duration(SessionId, Duration)),
        Duration >= MinDuration), AllSessionIds),
    sublist_offset_count(AllSessionIds, Offset, Count, SessionIds),
    maplist(session_data(Module), SessionIds, Sessions).

% All given user sessions.

bc_analytics_user_sessions(Module, UserId, Sessions):-
    findall(SessionId,
        call(Module:session_user(SessionId, UserId)), SessionIds),
    maplist(session_data(Module), SessionIds, Sessions).

% All given session pageviews.

bc_analytics_session_pageviews(Module, SessionId, Pageviews):-
    findall(PageviewId,
        call(Module:pageview_session(PageviewId, SessionId)), PageviewIds),
    maplist(pageview_data(Module), PageviewIds, Pageviews).

% Extracts sublist by offset and count.

sublist_offset_count(List, Offset, Count, Sublist):-
    UpperBound is Offset + Count,
    findall(Member, (
        nth0(Index, List, Member),
        Index >= Offset,
        Index < UpperBound), Sublist), !.

% Turns user id into a data dict containing
% information about the user.

user_data(Module, UserId, Dict):-
    call(Module:user_duration(UserId, Duration)),
    call(Module:user_timestamp(UserId, TimeStamp)),
    Dict = user{
        user_id: UserId,
        duration: Duration,
        timestamp: TimeStamp}.

% Turns user id into a data dict containing
% information about the user.

session_data(Module, SessionId, Dict):-
    call(Module:session_user(SessionId, UserId)),
    call(Module:session_duration(SessionId, Duration)),
    call(Module:session_timestamp(SessionId, TimeStamp)),
    call(Module:session_pagecount(SessionId, PageCount)),
    call(Module:session_agent(SessionId, Agent)),
    call(Module:session_platform(SessionId, Platform)),
    Dict = session{
        session_id: SessionId,
        user_id: UserId,
        duration: Duration,
        timestamp: TimeStamp,
        pagecount: PageCount,
        agent: Agent,
        platform: Platform}.

% Turns pageview id into a data dict containing
% information about the pageview.

% TODO: add title
pageview_data(Module, PageviewId, Dict):-
    call(Module:pageview_duration(PageviewId, Duration)),
    call(Module:pageview_timestamp(PageviewId, TimeStamp)),
    call(Module:pageview_location(PageviewId, Location)),
    call(Module:pageview_referrer(PageviewId, Referrer)),
    Dict = pageview{
        pageview_id: PageviewId,
        duration: Duration,
        timestamp: TimeStamp,
        location: Location,
        referrer: Referrer}.
