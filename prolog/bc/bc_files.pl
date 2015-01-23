:- module(bc_files, [
    directory_only_files/2, % +Directory, -Files
    remove_directory/1      % +Directory
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

% Removes the given directory
% recursively.

remove_directory(Directory):-
    directory_files(Directory, Entries),
    exclude(ignored_file, Entries, Files),
    maplist(join_directory(Directory), Files, Joined),
    maplist(remove_directory_entry, Joined),
    delete_directory(Directory).

ignored_file('.').
ignored_file('..').

join_directory(Directory, Entry, Joined):-
    atomic_list_concat([Directory, '/', Entry], Joined).

remove_directory_entry(Entry):-
    (   exists_directory(Entry)
    ->  remove_directory(Entry)
    ;   delete_file(Entry)).
