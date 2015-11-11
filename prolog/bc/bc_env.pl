:- module(bc_env, [
    bc_env/1,           % -Environment:atom
    bc_env_production/0
]).

/** <module> Helper for querying current environment
*/

bc_env(Env):-
    getenv('PL_ENV', Env), !.

bc_env(development).

% Succeeds when the current
% environment is production.

bc_env_production:-
    bc_env(production).
