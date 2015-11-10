:- module(bc_api_auth, [
    bc_auth/1,            % :Next
    bc_auth_user_by_key/1 % -User
]).

:- use_module(library(http/http_wrapper)).
:- use_module(library(dict_schema)).
:- use_module(library(docstore)).
:- use_module(library(arouter)).

:- use_module(bc_data).
:- use_module(bc_api_io).
:- use_module(bc_api_error).
:- use_module(bc_data_user).
:- use_module(bc_api_actor).

%! bc_auth(:Next) is det.
%
% Pre-action for handlers that need
% the user to be authenticated.

:- meta_predicate(bc_auth(0)).

bc_auth(Next):-
    (   bc_auth_user_by_key(User)
    ->  setup_call_cleanup(
            bc_set_actor(User),
            bc_call_handle_error(Next),
            bc_unset_actor)
    ;   bc_handle_error(error(invalid_api_key))).

% Authenticates the current user
% by the API key given in the "X-Key"
% HTTP header.

bc_auth_user_by_key(User):-
    http_current_request(Request),
    memberchk(x_key(Key), Request),
    ds_find(user, key=Key, [User]).

% Authenticates the user.

:- route_post(api/auth,
    bc_call_handle_error, user_auth).

user_auth:-
    bc_read_by_schema(bc_user_auth, Auth),
    bc_user_auth(Auth, Info),
    bc_reply_success(Info).

% Schema for authentication requests.

:- register_schema(bc_user_auth, _{
    type: dict,
    tag: user,
    keys: _{
        username: _{ type: atom, min_length: 1 },
        password: _{ type: atom, min_length: 1 }
    }
}).
