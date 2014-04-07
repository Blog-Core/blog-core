:- module(bc_api_auth, [
    bc_auth/1 % :Next
]).

:- use_module(library(http/http_wrapper)).
:- use_module(library(dict_schema)).
:- use_module(library(docstore)).
:- use_module(library(arouter)).

:- use_module(bc_data).
:- use_module(bc_api_io).
:- use_module(bc_api_error).
:- use_module(bc_data_user).
:- use_module(bc_data_cur_user).

%! bc_auth(:Next) is det.
%
% Pre-action for handlers that need
% the user to be authenticated.

:- meta_predicate(bc_auth(0)).

bc_auth(Next):-
    (   auth_user_by_key(User)
    ->  setup_call_cleanup(
            bc_set_user(User),
            bc_call_handle_error(Next),
            bc_unset_user)
    ;   bc_handle_error(error(invalid_api_key))).

% Authenticates the current user
% by the API key given in the "X-Key"
% HTTP header.

auth_user_by_key(User):-
    http_current_request(Request),
    memberchk(x_key(Key), Request),
    ds_find(user, key=Key, [User]).

% Authenticates the user.

:- route_post(api/auth,
    bc_call_handle_error, user_auth).

user_auth:-
    bc_read_by_schema(user_auth, Auth),
    bc_user_auth(Auth, Key),
    bc_reply_success(Key).

% Schema for authentication requests.

:- register_schema(user_auth, _{
    type: dict,
    tag: user,
    keys: _{
        username: _{ type: atom, min_length: 1 },
        password: _{ type: atom, min_length: 6 }
    }
}).
