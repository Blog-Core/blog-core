:- module(bc_files, [
    directory_only_files/2 % +Directory, -Files
]).

% Finds directory entries that
% are files.

directory_only_files(Directory, Files):-
    directory_files(Directory, Entries),
    exclude(not_file(Directory), Entries, Files), !.

not_file(_, '.').
not_file(_, '..').
not_file(Base, Directory):-
    atomic_list_concat([Base, '/', Directory], Path),
    exists_directory(Path).
