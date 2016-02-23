var fs = require('fs');
var api = require('../api');
var view = require('../view');
var message = require('../message');
var resolveObject = require('../resolve_object');

var template = fs.readFileSync(__dirname + '/config.html', { encoding: 'utf8' });

// Page for editing email settings.

exports.create = function(id) {

    var model = {

        permission: ko.observable(false),

        editing: ko.observable(false),

        config: ko.observable({}),

        form: {

            site: ko.observable(''),

            title: ko.observable('')
        },

        errors: {

            site: ko.observableArray([]),

            title: ko.observableArray([])
        }
    };

    // Saves settings and updates the
    // read-only views of options.

    model.save = function(form) {

        clearErrors();

        validateParams();

        // Checks whether some input
        // has error set.

        if (!checkError(form)) {

            return false;
        }

        var config = toConfig(model.form);

        api.updateConfig(toList(config)).then(function() {

            model.config(config);

            message.info('Configuration updated.');

            route.refresh();

        }).catch(message.error);
    };

    model.edit = function() {

        model.editing(true);
    };

    // Cancels editing. Refreshes current page.

    model.cancel = function() {

        route.refresh();
    };

    // Clears current form errors.

    function clearErrors() {

        Object.keys(model.errors).forEach(function(key) {

            model.errors[key]([]);
        });
    }

    // Validates parameters.

    function validateParams() {

        var form = model.form;

        if (form.site() === '') {

            model.errors.site.push('Site base URL must be set.');

        } else {

            if (!form.site().match(/^https?:\/\//)) {

                model.errors.site.push('Site base URL must start with http:// or https://.');
            }

            if (form.site().match(/\/$/)) {

                model.errors.site.push('Site base URL must not end with /.');
            }
        }
    }

    // Checks for errors on the form
    // elements. Focuses on the first
    // input with an error.

    function checkError(form) {

        var input = form.querySelector(
            '.has-error input, .has-error textarea,' +
            ' .has-error checkbox, .has-error select');

        if (input) {

            input.focus();

            return false;
        }

        return true;
    }

    return api.userInfo().then(function(info) {

        if (info.type !== 'admin') {

            view.show(template, model);

        } else {

            return api.config().then(function(data) {

                var config = extract(data);

                model.config(config);

                toForm(config, model.form);

                model.permission(true);

                view.show(template, model);
            });
        }
    });
};

// Extracts supported config settings from
// generic config entries.

function extract(config) {

    var obj = {};

    config.forEach(function(entry) {

        obj[entry.name] = entry.value;
    });

    return {

        site: obj.site,

        title: obj.title
    };
}

// Sets form values based on
// config values.

function toForm(config, form) {

    Object.keys(config).forEach(function(key) {

        form[key](config[key]);
    });
}

// Creates new object with
// values from the form binding
// object.

function toConfig(form) {

    var config = {};

    Object.keys(form).forEach(function(key) {

        config[key] = form[key]();
    });

    return config;
}

// Turns the configuration
// object into a list of configuration
// entries.

function toList(config) {

    return [

        { name: 'site', value: config.site },
        { name: 'title', value: config.title }
    ];
}
