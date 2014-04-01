:- module(bc_api, []).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).

:- use_module(library(arouter)).
:- use_module(library(dict_schema)).
:- use_module(library(docstore)).

:- use_module(bc_data).
:- use_module(bc_schema).
:- use_module(bc_view).

% Pre-action for handlers that need
% the user to be authenticated.

:- meta_predicate(auth_admin(0)).

auth(Next):-
    (   auth_user_by_key(User)
    ->  setup_call_cleanup(
            bc_set_user(User),
            call_handle_error(Next),
            bc_unset_user)
    ;   handle_error(error(invalid_api_key))).

% Calls handler through error cather
% that produces JSON reply on recognized errors.

:- meta_predicate(call_handle_error(0)).

call_handle_error(Goal):-
    catch(Goal, Error, true),
    (   var(Error)
    ;   handle_error(Error)), !.

% Handles known errors and produces
% approriate API response.

handle_error(error(invalid_api_key)):- !,
    reply_error('Invalid or missing API key.').

handle_error(error(invalid_input(Errors))):- !,
    format(atom(Message), 'Invalid input: ~w.', [Errors]),
    reply_error(Message).

handle_error(error(invalid_credentials)):- !,
    reply_error('Invalid auth credentials.').

handle_error(error(existing_slug(_))):- !,
    reply_error('Post with the same slug exists already.').

handle_error(Error):-
    throw(Error).

% Authenticates the current user
% by the API key given in the "X-Key"
% HTTP header.

auth_user_by_key(User):-
    http_current_request(Request),
    memberchk(x_key(Key), Request),
    ds_find(user, key=Key, [User]).

% Creation of new users.

:- route_post(api/user, auth, user_save).

user_save:-
    read_by_schema(user, User),
    bc_user_save(User, Id),
    reply_success(Id).

% Updates given user.

:- route_put(api/user/Id, auth, user_update(Id)).

user_update(Id):-
    read_by_schema(user, User),
    bc_user_update(Id, User),
    reply_success(Id).

% Removes an user.

:- route_del(api/user/Id, auth, user_remove(Id)).

user_remove(Id):-
    bc_user_remove(Id),
    reply_success(Id).

% Authenticates the user.

:- route_post(api/auth, call_handle_error, user_auth).

user_auth:-
    read_by_schema(user_auth, Auth),
    bc_user_auth(Auth, Key),
    reply_success(Key).

% Adds new post.

:- route_post(api/post, auth, post_save).

post_save:-
    read_by_schema(post, Post),
    bc_post_save(Post, Id),
    bc_view_purge_cache,
    reply_success(Id).

% Updates the given post.

:- route_put(api/post/Id, auth, post_update(Id)).

post_update(Id):-
    read_by_schema(post, Post),
    bc_post_update(Id, Post),
    bc_view_purge_cache,
    reply_success(Id).

% Removes the post.

:- route_del(api/post/Id, auth, post_remove(Id)).

post_remove(Id):-
    bc_post_remove(Id),
    bc_view_purge_cache,
    reply_success(Id).

% List of all posts.
% FIXME add comment count.

:- route_get(api/posts, auth, post_list).

post_list:-
    bc_post_list(List),
    reply_success(List).

% Single post with contents.

:- route_get(api/post/Id, auth, post_get(Id)).

post_get(Id):-
    bc_post(Id, Post),
    reply_success(Post).

% Gets config values.

:- route_get(api/configs, auth, config_list).

config_list:-
    bc_config_list(List),
    reply_success(List).

% Updates the config value.

:- route_put(api/config, auth, config_update).

config_update:-
    read_by_schema(config, Config),
    get_dict_ex(name, Config, Name),
    get_dict_ex(value, Config, Value),
    bc_config_set(Name, Value),
    reply_success(Name).

% Comments of a single post.

:- route_get(api/post/Id/comments, auth, comment_list(Id)).

comment_list(PostId):-
    bc_comment_list(PostId, Comments),
    reply_success(Comments).

% Adds new comment. This is available for
% everyone.

:- route_post(api/post/Id/comment, comment_save(Id)).

comment_save(PostId):-
    read_by_schema(comment, Comment),
    bc_comment_save(PostId, Comment),
    bc_view_purge_cache,
    reply_success(PostId).

% TODO modify comment.

% Helper that reads dict from JSON request
% and validates it against the schema.

read_by_schema(Schema, Dict):-
    http_current_request(Request),
    http_read_json_dict(Request, Raw),
    convert(Raw, Schema, Dict, Errors),
    (   Errors = []
    ;   throw(error(invalid_input(Errors)))), !.

% Sends JSON response with Data
% and success.

reply_success(Data):-
    reply_json(_{ status: success, data: Data }).

% Sends error JSON response with Message.

reply_error(Message):-
    reply_json(_{ status: error, message: Message }).
