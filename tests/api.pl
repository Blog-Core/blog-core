:- begin_tests(api).

:- use_module(library(docstore)).
:- use_module(util).

test('GET /api/configs', [setup(new_database)]):-
    request_get('/api/configs', Dict),
    Dict.status = "success",
    Dict.data = List,
    assertion(is_list(List)),
    List = [TitleConfig],
    TitleConfig.name = _,
    TitleConfig.value = _.

test('PUT /api/config', [setup(new_database)]):-
    request_put('/api/config', _{
        name: "title",
        value: "Updated title"
    }, Dict),
    Dict.status = "success".

:- end_tests(api).
