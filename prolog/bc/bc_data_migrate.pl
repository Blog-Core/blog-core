:- module(bc_data_migrate, [
    bc_data_migrate/3 % +Name, +Description, :Goal
]).

/** <Module> Data migrations

Handles data migrations for the underlying docstore database.
Migrations are run in the order of bc_data_migrate/3 calls.
*/

:- use_module(library(docstore)).
:- use_module(library(debug)).

:- meta_predicate(bc_data_migrate(+, +, 0)).

%! bc_data_migrate(+Name, +Desc, :Goal) is det.
%
% Executes the given migration in transaction.
% Remembers the migration by it's name. Migration
% names must be unique.

% FIXME take empty subset of keys.

bc_data_migrate(Name, Description, Goal):-
    (   ds_find(migration, name=Name, [_])
    ->  true
    ;   Transaction = (Goal, bc_remember_migration(Name, Description)),
        debug(bc_data_migrate, 'running migration ~p', [Name]),
        ds_transactional(Transaction)).

%! remember_migration(+Name, +Description) is det.
%
% Records the entry that the given migration has been run.

bc_remember_migration(Name, Description):-
    ds_insert(migration{ name: Name, description: Description }).
