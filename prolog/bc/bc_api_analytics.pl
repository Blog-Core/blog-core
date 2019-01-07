:- module(bc_api_analytics, []).

/** <module> HTTP handlers for managing posts */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(url)).

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_view).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_data_entry).
:- use_module(bc_analytics).
:- use_module(bc_analytics_db).
:- use_module(bc_admin_file).

% Handlers for data from the readers script.

:- route_post(api/readers/user, record_user).

record_user:-
    bc_read_by_schema(bc_analytics_user, User),
    bc_analytics_record_user(User, UserId),
    bc_reply_success(UserId).

:- route_post(api/readers/session, record_session).

record_session:-
    bc_read_by_schema(bc_analytics_session, Session),
    bc_analytics_record_session(Session, SessionId),
    bc_reply_success(SessionId).

:- route_post(api/readers/pageview, record_pageview).

record_pageview:-
    bc_read_by_schema(bc_analytics_pageview, Pageview),
    bc_analytics_record_pageview(Pageview, PageviewId),
    bc_reply_success(PageviewId).

:- route_post(api/readers/pageview_extend, record_pageview_extend).

record_pageview_extend:-
    bc_read_by_schema(bc_analytics_pageview_extend, Pageview),
    bc_analytics_record_pageview_extend(Pageview),
    bc_reply_success(true).

% Full tracking script.

:- route_get(bc/'readers.min.js', visitor_script).

visitor_script:-
    bc_admin_send_file('js/readers.min.js').

:- route_get(bc/'readers.min.js.map', visitor_script_map).

visitor_script_map:-
    bc_admin_send_file('js/readers.min.js.map').

% Pixel-based tracking script.

:- route_get(bc/'readers.image.min.js', visitor_image_script).

visitor_image_script:-
    bc_admin_send_file('js/readers.image.min.js').

:- route_get(bc/'readers.image.min.js.map', visitor_image_script_map).

visitor_image_script_map:-
    bc_admin_send_file('js/readers.image.min.js.map').

:- route_get(bc/'reader.png', visitor_pixel).

visitor_pixel:-
    http_current_request(Request),
    pixel_location(Request, Location),
    pixel_agent(Request, Agent),
    memberchk(search(Query), Request),
    param_or_null(u, Query, UserId),
    param_or_null(s, Query, SessionId),
    param_or_null(p, Query, Platform),
    param_or_null(t, Query, Title),
    param_or_null(e, Query, EntryId),
    param_or_null(r, Query, Referrer),
    Data = pixel{
        user_id: UserId,
        agent: Agent,
        platform: Platform,
        session_id: SessionId,
        location: Location,
        referrer: Referrer,
        entry_id: EntryId,
        title: Title
    },
    bc_analytics_record_pixel(Data),
    bc_admin_relative(img/'reader.png', ImagePath),
    http_reply_file(ImagePath, [unsafe(true), cache(false)], Request).

param_or_null(Name, Query, Value):-
    memberchk(Name=Value, Query), !.

param_or_null(_, _, null).

pixel_location(Request, Path):-
    memberchk(referer(Referrer), Request), !,
    parse_url(Referrer, Attributes),
    memberchk(path(Path), Attributes).

pixel_location(_, null).

pixel_agent(Request, Agent):-
    memberchk(user_agent(Agent), Request), !.

pixel_agent(_, null).

:- register_schema(bc_analytics_user, _{
    type: dict,
    tag: user,
    keys: _{}
}).

:- register_schema(bc_analytics_session, _{
    type: dict,
    tag: session,
    keys: _{
        user_id: atom,
        agent: atom,
        platform: atom
    }
}).

:- register_schema(bc_analytics_pageview, _{
    type: dict,
    tag: pageview,
    keys: _{
        session_id: atom,
        location: atom,
        referrer: atom,
        elapsed: integer,
        entry_id: atom,
        title: atom
    }
}).

:- register_schema(bc_analytics_pageview_extend, _{
    type: dict,
    tag: pageview_extend,
    keys: _{
        pageview_id: atom,
        elapsed: integer
    }
}).

% Analytic timeseries results for the administration API.

:- route_get(api/analytics/timeseries/From/To/Duration,
   bc_auth, analytics_timeseries(From, To, Duration)).

% TODO: check atom_number/2 calls.

analytics_timeseries(From, To, Duration):-
    atom_number(Duration, DurationNum),
    parse_month(From, FromParsed),
    parse_month(To, ToParsed),
    bc_analytics_user_ts(FromParsed-ToParsed, DurationNum, Users),
    bc_analytics_session_ts(FromParsed-ToParsed, DurationNum, Sessions),
    bc_analytics_pageview_ts(FromParsed-ToParsed, DurationNum, Pageviews),
    bc_reply_success(_{
        users: Users,
        sessions: Sessions,
        pageviews: Pageviews}).

% Analytics summary.

:- route_get(api/analytics/summary/From/To/Duration,
   bc_auth, analytics_summary(From, To, Duration)).

analytics_summary(From, To, Duration):-
    atom_number(Duration, DurationNum),
    parse_month(From, FromParsed),
    parse_month(To, ToParsed),
    bc_analytics_summary(FromParsed-ToParsed,
        DurationNum, Summary),
    bc_reply_success(Summary).

% List of recent users.

:- route_get(api/analytics/users/From/To/Duration/Offset/Count,
   bc_auth, analytics_users(From, To, Duration, Offset, Count)).

analytics_users(From, To, Duration, Offset, Count):-
    atom_number(Duration, DurationNum),
    parse_month(From, FromParsed),
    parse_month(To, ToParsed),
    atom_number(Offset, OffsetNum),
    atom_number(Count, CountNum),
    bc_analytics_users(FromParsed-ToParsed,
        DurationNum, OffsetNum, CountNum, Users),
    bc_reply_success(Users).

% List of top pages.

:- route_get(api/analytics/pages/From/To/Duration,
   bc_auth, analytics_pages(From, To, Duration)).

analytics_pages(From, To, Duration):-
    atom_number(Duration, DurationNum),
    parse_month(From, FromParsed),
    parse_month(To, ToParsed),
    bc_analytics_top_pages(FromParsed-ToParsed,
        DurationNum, Pages),
    bc_reply_success(Pages).

parse_month(Atom, (YearNum, MonthNum)):-
    atomic_list_concat([Year, Month], -, Atom),
    atom_number(Year, YearNum),
    atom_number(Month, MonthNum).