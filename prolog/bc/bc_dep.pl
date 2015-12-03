:- module(bc_dep, [
    bc_check_dependencies/0
]).

/** <module> Checks the dependency versions
*/

:- use_module(library(error)).

bc_check_dependencies:-
    check_swi,
    check_installed(smtp, v(0, 9, 4)),
    check_installed(docstore, v(2, 0, 1)),
    check_installed(arouter, v(1, 1, 1)),
    check_installed(simple_template, v(1, 0, 1)),
    check_installed(sort_dict, v(0, 0, 3)),
    check_installed(dict_schema, v(0, 0, 2)),
    check_installed(markdown, v(0, 0, 2)).

check_swi:-
    current_prolog_flag(version, Version),
    (   Version < 70312
    ->  throw(error(bc_dep:old_swi_version(Version), _))
    ;   true).

% Checks that the given version is
% installed. Throws an error when
% it is not.

check_installed(Pack, Pattern):-
    must_be(atom, Pack),
    (   pack_property(Pack, version(Atom))
    ->  true
    ;   throw(error(bc_dep:pack_not_installed(Pack), _))),
    parse_version(Pack, Atom, Version),
    (   check_version(Pattern, Version)
    ->  true
    ;   throw(error(bc_dep:pack_version_mismatch(
            Pack, Pattern, Version), _))).

% Checks that the version matches the
% pattern.

check_version(Pattern, Version):-
    must_be(nonvar, Pattern),
    must_be(nonvar, Version),
    Pattern = v(Major, Minor, Patch),
    Version = v(CheckMajor, CheckMinor, CheckPatch),
    (   nonvar(Major)
    ->  CheckMajor = Major,
        (   nonvar(Minor)
        ->  (   CheckMinor = Minor
            ->  (   nonvar(Patch)
                ->  CheckPatch >= Patch
                ;   true)
            ;   (   CheckMinor < Minor
                ->  fail
                ;   true))
        ;   true)
    ;   true).

% Parses version string like '0.0.9'
% into a term v(0, 0, 9).

parse_version(Pack, Atom, Term):-
    must_be(atom, Pack),
    must_be(atom, Atom),
    (   atomic_list_concat(Tokens, '.', Atom),
        Tokens = [MajorAtom, MinorAtom, PatchAtom],
        atom_number(MajorAtom, Major),
        atom_number(MinorAtom, Minor),
        atom_number(PatchAtom, Patch),
        integer(Major),
        integer(Minor),
        integer(Patch)
    ->  Term = v(Major, Minor, Patch)
    ;   throw(error(bc_dep:invalid_pack_version(Pack, Atom), _))).

% Provides messages to terminal.

:- multifile(prolog:message//1).

% When pack is not installed at all.

prolog:message(error(bc_dep:pack_not_installed(
        Pack), _)) -->
    ['Pack ~w is not installed.'-[Pack]].

% When pack is installed but it has
% non-parseable version.

prolog:message(error(bc_dep:invalid_pack_version(
        Pack, Atom), _)) -->
    ['Pack ~w has invalid version ~w.'-[Pack, Atom]].

% When the SWI-Prolog has too old version.

prolog:message(error(bc_dep:old_swi_version(
        Version), _)) -->
    {   Major is Version div 10000,
        Minor is (Version - Major * 10000) div 100,
        Patch is Version rem 100 },
    ['SWI-Prolog version ~w.~w.~w is too old.'-[Major, Minor, Patch]].

% When the pack does not satisfy the
% required version.

prolog:message(error(bc_dep:pack_version_mismatch(
        Pack, Pattern, Version), _)) -->
    ['Pack ~w has version ~w but required is ~w.'-[Pack, Version, Pattern]].
