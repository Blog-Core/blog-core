// Trim shim for IE8.

if (typeof String.prototype.trim !== 'function') {

    String.prototype.trim = function() {

        return this.replace(/^\s+|\s+$/g, '');
    };
}

// Helper for API queries.

var api = {

    // Retrieves captcha question.

    question: function(cb) {

        var xhr = new XMLHttpRequest();

        xhr.open('GET', '/api/question', true);

        xhr.onreadystatechange = api.ready(xhr, cb);

        xhr.send();
    },

    // Sends new comment.

    post: function(id, values, cb) {

        var xhr = new XMLHttpRequest();

        var url = '/api/post/' + encodeURIComponent(id) + '/comment';

        xhr.open('POST', url, true);

        xhr.setRequestHeader('Content-Type', 'application/json');

        xhr.onreadystatechange = api.ready(xhr, cb);

        xhr.send(JSON.stringify(data));
    },

    // Returns function for state change.

    ready: function(xhr, cb) {

        return function() {

            if (xhr.readyState === 4) {

                if (xhr.status === 200) {

                    cb(null, JSON.parse(xhr.responseText));

                } else {

                    cb(new Error('API error.'));
                }
            }
        }
    }
};

// Helper to extract values from input elements.

var input = {

    string: function(element) {

        return element ? element.value.trim() : undefined;
    },

    check: function(element) {

        return element ? element.checked : false;
    },

    int: function(element) {

        return element ? parseInt(element.value, 10) : undefined;
    }
};

// Helpers to validate data.

var validator = {

    // String validators.

    string: {

        is_set: function(value, list) {

            if (typeof value === 'undefined' ||
                value.length === 0) {

                list.push('__ must be set.');
            }
        },

        email: function(value, list) {

            if (typeof value === 'string' &&
                value !== '' &&
                !value.match(/[^@]+@[^@]+/)) {

                list.push('__ must match email pattern.');
            }
        },

        site: function(value, list) {

            if (typeof value === 'string' &&
                value !== '' &&
                !value.match(/https?:\/\//)) {

                list.push('__ must match URL pattern.');
            }
        }
    },

    // Checks array of validators.

    check: function(row, key, value) {

        var list = [];

        var validators = row.validators || [];

        for (var i = 0; i < validators.length; i++) {

            validators[i].call(null, value, list);
        }

        var label = key.substring(0, 1).toUpperCase() +
            key.substring(1);

        for (var j = 0; j < list.length; j++) {

            list[j] = list[j].replace('__', label);
        }

        return list;
    }
};

// Helper to work with DOM classes.

var clazz = {

    list: function(element) {

        return element.className.split(/\s+/);
    },

    // Removes element class.

    remove: function(element, cls) {

        var list = clazz.list(element);

        var removed = [];

        for (var i = 0; i < list.length; i++) {

            if (list[i] !== cls && list[i] !== '') {

                removed.push(list[i]);
            }
        }

        element.className = removed.join(' ');
    },

    // Adds element class.

    add: function(element, cls) {

        var list = clazz.list(element);

        var found = false;

        for (var i = 0; i < list.length; i++) {

            if (list[i] === cls) {

                found = true;
            }
        }

        if (!found) {

            list.push(cls);
        }

        element.className = list.join(' ');
    }
};

// Helper to manage form errors.

var errors = {

    // Adds new error.

    display: function(row, message) {

        clazz.add(row.element.parentNode, 'error');

        row.error = document.createElement('div');

        row.error.innerHTML = message;

        row.element.parentNode.appendChild(row.error);
    },

    // Removes all errors.

    remove: function(row) {

        var type = row.element.getAttribute('type');

        // Ignore hidden inputs.

        if (type !== 'hidden') {

            clazz.remove(row.element.parentNode, 'error');
        }

        if (row.error) {

            row.error.parentNode.removeChild(row.error);

            delete row.error;
        }
    }
};

// Helper that processes inputs object.

var reader = {

    // Reads data from the form descriptor.

    read: function(desc) {

        var valid = true;

        var values = {};

        reader.each(desc, function(row, key) {

            errors.remove(row);

            var value = input[row.type](row.element);

            var list = validator.check(row, key, value);

            if (list.length > 0) {

                valid = false;

                errors.display(row, list.join(' '));
            }

            if (typeof value !== 'undefined') {

                values[key] = value;
            }
        });

        return { values: values, valid: valid };
    },

    // Runs callback for each input.

    each: function(desc, cb) {

        for (var key in desc) {

            var row = desc[key];

            // Consider only defined elements.

            if (row.element) {

                cb(row, key);
            }
        }
    }
};

module.exports = function(options) {

    if (typeof options.form === 'string') {

        options.form = document.querySelector(options.form);
    }

    if (typeof options.question === 'string') {

        options.question = document.querySelector(options.question);
    }

    if (!options.form) {

        throw new Error('Comment form element not specified.');
    }

    if (!options.question) {

        throw new Error('Verification question element not specified.');
    }

    if (!options.message) {

        throw new Error('Feedback message/error element not specified.');
    }

    var form = options.form;

    var elements = form.elements;

    var data = {

        post_id: {

            type: 'string',

            element: elements.post_id
        },

        reply_to: {

            type: 'string',

            element: elements.reply_to
        },

        author: {

            type: 'string',

            validators: [ validator.string.is_set ],

            element: elements.author
        },

        email: {

            type: 'string',

            validators: [ validator.string.email ],

            element: elements.email
        },

        site: {

            type: 'string',

            validators: [ validator.string.site ],

            element: elements.site
        },

        content: {

            type: 'string',

            validators: [ validator.string.is_set ],

            element: elements.content
        },

        notify: {

            type: 'check',

            element: elements.notify
        },

        question: {

            type: 'int',

            element: elements.question
        },

        answer: {

            type: 'string',

            validators: [ validator.string.is_set ],

            element: elements.answer
        }
    };

    api.question(function(err, response) {

        if (err) {


        } else {

            // TODO check that question element is set.
            // TODO check response status

            options.question.innerHTML = response.data.question;
        }
    });

    form.onsubmit = function() {

        var result = reader.read(data);

        console.log(JSON.stringify(result, null, 2));

        return false;
    };
};
