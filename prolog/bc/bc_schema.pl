:- module(bc_schema, []).

/** <module> API data schemas

The module defines schemas for API data.
*/

:- use_module(library(dict_schema)).

:- register_schema(user, _{
    type: dict,
    tag: user,
    keys: _{
        fullname: _{ type: atom, min_length: 1 },
        username: _{ type: atom, min_length: 1 },
        password: _{ type: atom, min_length: 6 },
        type: _{ type: enum, values: [author, admin] }
    }
}).

:- register_schema(user_auth, _{
    type: dict,
    tag: user,
    keys: _{
        username: _{ type: atom, min_length: 1 },
        password: _{ type: atom, min_length: 6 }
    }
}).

:- register_schema(post, _{
    type: dict,
    tag: post,
    keys: _{
        author: _{ type: atom, min_length: 36, max_length: 36 },
        title: _{ type: string, min_length: 1 },
        slug: _{ type: atom, min_length: 1 },
        tags: _{ type: list, items: atom },
        date_published: _{ type: integer, min: 0 },
        date_updated: _{ type: integer, min: 0 },
        commenting: bool,
        published: bool,
        content: string,
        content_type: _{ type: enum, values: [markdown, raw] },
        description: string,
        type: _{ type: enum, values: [page, post, block] }
    },
    optional: [ author ]
}).

% Generic config entry.

:- register_schema(config, _{
    type: dict,
    tag: config,
    keys: _{
        name: atom,
        value: [ atom, number ]
    }
}).

% Basic comments. Can be overriden to
% add more properties.

:- register_schema(comment, _{
    type: dict,
    tag: comment,
    keys: _{
        author: _{ type: string, min_length: 1 },
        content: _{ type: string, min_length: 1 },
        date: _{ type: integer, min: 0 }
    },
    optional: [date]
}).
