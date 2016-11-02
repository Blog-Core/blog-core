:- module(bc_image, [
    bc_image_dimensions/3 % +Path, -Width, -Height
]).

/** <module> Helper module to obtain image dimensions */

:- use_module(library(process)).
:- use_module(library(dcg/basics)).

% Extracts image dimensions. Fails when
% ImageMagick's identify command is not
% found in PATH or outputs unexpected data.

bc_image_dimensions(Path, Width, Height):-
    catch(absolute_file_name(path(identify),
        Identify, [access(execute)]), _, fail),
    setup_call_cleanup(
        process_create(Identify,
            ['-format', '%[fx:w]x%[fx:h]', Path], [stdout(pipe(Out))]),
        read_stream_to_codes(Out, Codes),
        close(Out)),
    parse_dimensions(Codes, Width, Height),
    Width > 0, Height > 0.

parse_dimensions(Codes, Width, Height):-
    phrase(dcg_dimensions(Width, Height), Codes, _), !.

dcg_dimensions(Width, Height) -->
    integer(Width), "x", integer(Height).
