:- module(bc_api_analytics, []).

/** <module> HTTP handlers for managing posts */

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).

:- use_module(bc_view).
:- use_module(bc_api_io).
:- use_module(bc_api_auth).
:- use_module(bc_api_actor).
:- use_module(bc_data_entry).
:- use_module(bc_analytics).

:- route_post(api/visitor/user, record_user).

record_user:-
    bc_read_by_schema(bc_analytics_user, User),
    bc_record_user(User, UserId),
    bc_reply_success(UserId).

:- route_post(api/visitor/session, record_session).

record_session:-
    bc_read_by_schema(bc_analytics_session, Session),
    bc_record_session(Session, SessionId),
    bc_reply_success(SessionId).

:- route_post(api/visitor/pageview, record_pageview).

record_pageview:-
    bc_read_by_schema(bc_analytics_pageview, Pageview),
    bc_record_pageview(Pageview, PageviewId),
    bc_reply_success(PageviewId).

:- route_post(api/visitor/pageview_extend, record_pageview_extend).

record_pageview_extend:-
    bc_read_by_schema(bc_analytics_pageview_extend, Pageview),
    bc_record_pageview_extend(Pageview),
    bc_reply_success(true).

:- route_get(bc/'visitor.min.js', visitor_script).

visitor_script:-
    http_current_request(Request),
    module_property(bc_api_analytics, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/public/js/visitor.min.js', Path),
    http_reply_file(Path, [unsafe(true)], Request).

:- route_get(bc/'visitor.min.js.map', visitor_script_map).

visitor_script_map:-
    http_current_request(Request),
    module_property(bc_api_analytics, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/public/js/visitor.min.js.map', Path),
    http_reply_file(Path, [unsafe(true)], Request).

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
        elapsed: integer
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
