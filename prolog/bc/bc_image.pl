:- module(bc_image, [
    bc_image_dimensions/3 % +Path, -Width, -Height
]).

/** <module> Helper module to obtain image dimensions */

:- use_module(library(process)).
:- use_module(library(dcg/basics)).

% Extracts image dimensions. Fails when
% ImageMagick's identify command is not
% found in PATH.

bc_image_dimensions(Path, Width, Height):-
    catch(absolute_file_name(path(identify),
        Identify, [access(execute)]), _, fail),
    process_create(Identify,
        ['-format', '%[fx:w]x%[fx:h]', Path], [stdout(pipe(Out))]),
    read_stream_to_codes(Out, Codes),
    parse_dimensions(Codes, Width, Height).

parse_dimensions(Codes, Width, Height):-
    phrase(dcg_dimensions(Width, Height), Codes).

dcg_dimensions(Width, Height) -->
    integer(Width), "x", integer(Height).
