:- module(bc_hex, [
    bc_atom_hex/2, % +Atom, -HexAtom
    bc_hex_atom/2  % +HexAtom, -Atom
]).

:- use_module(library(memfile)).
:- use_module(library(readutil)).

%! bc_atom_hex(+Atom, -HexAtom) is det.
%
% Converts given atom into UTF-8-encoded
% hex atom.

bc_atom_hex(Atom, Hex):-
    atom_chars(Atom, InChars),
    setup_call_cleanup(
        new_memory_file(Handle),
        chars_to_hex(Handle, InChars, OutChars),
        free_memory_file(Handle)),
    atom_chars(Hex, OutChars).

%! bc_hex_atom(+HexAtom, -Atom) is det.
%
% Converts given hex atom into UTF-8-encoded
% atom. Throws error(cannot_convert_hex(Up, Low))
% when the hex atom cannot be converted.

bc_hex_atom(Hex, Atom):-
    atom_chars(Hex, InChars),
    setup_call_cleanup(
        new_memory_file(Handle),
        hex_to_chars(Handle, InChars, OutChars),
        free_memory_file(Handle)),
    atom_chars(Atom, OutChars).

% Runs conversion through the
% memory file.

hex_to_chars(Memfile, InChars, OutChars):-
    hex_to_memfile(InChars, Memfile),
    memfile_to_chars(Memfile, OutChars).

% Runs conversion through the
% memory file.

chars_to_hex(Memfile, InChars, OutChars):-
    chars_to_memfile(InChars, Memfile),
    memfile_to_hex(Memfile, OutChars).

% Writes the list of characters
% into the memory file.

chars_to_memfile(Chars, Memfile):-
    setup_call_cleanup(
        open_memory_file(Memfile, write, Stream, [ encoding(utf8) ]),
        chars_to_stream(Chars, Stream),
        close(Stream)).

% Writes the list of characters
% into the stream.

chars_to_stream([], _).

chars_to_stream([Char|Chars], Stream):-
    put_char(Stream, Char),
    chars_to_stream(Chars, Stream).

% Reads memfile into the list
% of characters.

memfile_to_chars(Memfile, Chars):-
    setup_call_cleanup(
        open_memory_file(Memfile, read, Stream, [ encoding(utf8) ]),
        stream_to_chars(Stream, Chars),
        close(Stream)).

% Reads stream into the list
% of characters.

stream_to_chars(Stream, Chars):-
    get_char(Stream, Char),
    (   Char = end_of_file
    ->  Chars = []
    ;   Chars = [Char|Rest],
        stream_to_chars(Stream, Rest)).

% Reads memfile into the list
% of hex characters.

memfile_to_hex(Memfile, Out):-
    setup_call_cleanup(
        open_memory_file(Memfile, read, Stream, [ encoding(octet) ]),
        stream_to_hex(Stream, Out),
        close(Stream)).

% Writes the list of hex characters
% into the memfile.

hex_to_memfile(Chars, Memfile):-
    setup_call_cleanup(
        open_memory_file(Memfile, write, Stream, [ encoding(octet) ]),
        hex_to_stream(Chars, Stream),
        close(Stream)).

% Writes the list of hex characters
% into the stream.

hex_to_stream([], _).

hex_to_stream([_], _).

hex_to_stream([Up, Low|Chars], Stream):-
    hex(Hex),
    (   nth0(Upper, Hex, Up),
        nth0(Lower, Hex, Low)
    ->  Byte is Upper << 4 + Lower,
        put_byte(Stream, Byte)
    ;   throw(error(cannot_convert_hex(Up, Low)))),
    hex_to_stream(Chars, Stream).

% Reads stream into the list
% of hex characters.

stream_to_hex(Stream, Out):-
    get_byte(Stream, Byte),
    (   Byte = -1
    ->  Out = []
    ;   hex(Hex),
        Upper is Byte >> 4,
        Lower is Byte /\ 15,
        nth0(Upper, Hex, Up),
        nth0(Lower, Hex, Low),
        Out = [Up,Low|Rest],
        stream_to_hex(Stream, Rest)).

% FIXME could have better representation?

hex(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']).
