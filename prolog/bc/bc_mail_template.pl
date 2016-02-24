:- module(bc_mail_template, [
    bc_mail_register_template/3, % +Name, +Subject, +Text
    bc_mail_render_template/3    % +Name, +Data, -Dict
]).

:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(st/st_render)).

:- use_module(bc_data_config).

:- dynamic(template/3).

%! bc_mail_register_template(+Name, +Subject, +Text) is det.
%
% Adds or replaces the given file.

bc_mail_register_template(Name, Subject, Text):-
    must_be(atom, Name),
    must_be(string, Subject),
    must_be(string, Text),
    retractall(template(Name, _, _)),
    assertz(template(Name, Subject, Text)),
    debug(bc_mail, 'registered mail template ~w', [Name]).

%! bc_mail_render_template(+Name, +Data, -Dict) is det.
%
% Renders the mail template with given data.

bc_mail_render_template(Name, Data, Dict):-
    must_be(atom, Name),
    must_be(dict, Data),
    (   template(Name, SubjectSource, BodySource)
    ->  true
    ;   throw(error(existence_error(mail_template, Name), _))),
    bc_config_dict(Config),
    RenderData = Data.put(config, Config),
    render_subject(Name, SubjectSource, RenderData, Subject),
    render_body(Name, BodySource, RenderData, Body),
    Dict = _{ subject: Subject, body: Body }.

% Renders subject line template.

render_subject(Name, Source, Data, Subject):-
    with_output_to(string(Subject), (
        current_output(Out),
        st_render_string(Source, Data,
            Out, mail/Name/subject, _{
            extension: txt,
            cache: true,
            strip: false,
            frontend: simple
        }))).

% Renders body template.

render_body(Name, Source, Data, Body):-
    with_output_to(string(Body), (
        current_output(Out),
        st_render_string(Source, Data,
            Out, mail/Name, _{
            extension: txt,
            cache: true,
            strip: false,
            frontend: simple
        }))).

% Reads mail template. File is relative
% to mail templates directory.

read_mail_template(File, Source):-
    mail_directory(Directory),
    read_file_to_string(Directory/File, Source, []).

% Mail templates directory.

mail_directory(Directory):-
    module_property(bc_mail_template, file(File)),
    file_directory_name(File, Dir),
    atom_concat(Dir, '/mail', Directory).

% Default mail template for mention mails.

:-  read_mail_template('mention.txt', Body),
    bc_mail_register_template(
        mention, "{{= entry.title }} - comment", Body).

% Default mail template for reply mails.

:-  read_mail_template('reply.txt', Body),
    bc_mail_register_template(
        reply, "{{= entry.title }} - reply", Body).

% Default mail template for comment notifications
% to the entry author.

:-  read_mail_template('comment.txt', Body),
    bc_mail_register_template(
        comment, "{{= entry.title }} - new comment", Body).
