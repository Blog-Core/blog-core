var fs = require('fs');
var api = require('../api');
var view = require('../view');
var email = require('../vm/email');
var message = require('../message');
var resolveObject = require('../resolve_object');

var template = fs.readFileSync(__dirname + '/email.html', { encoding: 'utf8' });

// Page for editing email settings.

exports.create = function(id) {

    var model = {

        email: ko.observable(),

        permission: ko.observable(false),

        editing: ko.observable(false),

        smtp: ko.observable({}),

        form: {

            enabled: ko.observable(false),

            host: ko.observable(''),

            user: ko.observable(''),

            password: ko.observable(''),

            auth: ko.observable('login'),

            security: ko.observable('none'),

            from: ko.observable('admin@example.com'),

            subject: ko.observable('Test mail'),

            body: ko.observable('Test mail body')
        },

        errors: {

            host: ko.observableArray([]),

            user: ko.observableArray([]),

            password: ko.observableArray([]),

            from: ko.observableArray([]),

            subject: ko.observableArray([]),

            body: ko.observableArray([])
        },

        // Test error.

        error: ko.observable(),

        // Test info message.

        info: ko.observable(),

        // Flag to show password unmasked.

        password_text: ko.observable(false)
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

        var smtp = toSmtp(model.form);

        api.updateConfig(toList(smtp)).then(function() {

            model.smtp(smtp);

            message.info('Mailing system parameters have been updated.');

            route.refresh();

        }).catch(message.error);
    };

    model.edit = function() {

        model.editing(true);
    };

    // Tests the settings that are currently
    // set in the editing form.

    model.test = function() {

        model.info(undefined);

        model.error(undefined);

        clearErrors();

        validateParams();

        validateTest();

        var form = document.getElementById('smtp-config');

        // Checks whether some input
        // has error set.

        if (!checkError(form)) {

            return false;
        }

        var smtp = toSmtp(model.form);

        api.testMail(smtp).then(function() {

            model.info('Test mail sent successfully.');

        }).catch(function(err) {

            model.error('Mail testing failed: ' +
                (err.jsendMessage ? err.jsendMessage : err.toString()));
        });
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

    // Validates SMTP parameters.

    function validateParams() {

        var form = model.form;

        if (form.host().match(/^\s*$/)) {

            model.errors.host.push('SMTP host name must be set.');
        }

        if (form.auth() === 'login') {

            if (form.user().match(/^\s*$/)) {

                model.errors.user.push('SMTP user must be set for SMTP auth.');
            }

            if (form.password().match(/^\s*$/)) {

                model.errors.password.push('SMTP password must be set for SMTP auth.');
            }
        }

        if (form.from().match(/^\s*$/)) {

            model.errors.from.push('Notification sender mail must be set.');

        } else {

            if (!form.from().match(/^[^@]+@[^@]+$/)) {

                model.errors.from.push('Notification sender mail must be a valid mail address.');
            }
        }
    }

    // Validates test parameters.

    function validateTest() {

        var form = model.form;

        if (form.subject().match(/^\s*$/)) {

            model.errors.subject.push('Test mail subject must be set.');
        }

        if (form.body().match(/^\s*$/)) {

            model.errors.body.push('Test mail body must be set.');
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

            return api.config().then(function(config) {

                var smtp = extract(config);

                model.smtp(smtp);

                toForm(smtp, model.form);

                model.permission(true);

                view.show(template, model);
            });
        }
    });
};

// Extracts SMTP settings from
// generic config entries.

function extract(config) {

    var obj = {};

    config.forEach(function(entry) {

        obj[entry.name] = entry.value;
    });

    return {

        enabled: obj.smtp_enabled,

        host: obj.smtp_host,

        user: obj.smtp_user,

        password: obj.smtp_password,

        auth: obj.smtp_auth,

        security: obj.smtp_security,

        from: obj.smtp_from
    };
}

// Sets form values based on
// smtp values.

function toForm(smtp, form) {

    Object.keys(smtp).forEach(function(key) {

        form[key](smtp[key]);
    });
}

// Creates new object with
// values from the form binding
// object.

function toSmtp(form) {

    var smtp = {};

    Object.keys(form).forEach(function(key) {

        smtp[key] = form[key]();
    });

    return smtp;
}

// Turns the SMTP configuration
// object into a list of configuration
// entries.

function toList(smtp) {

    return [

        { name: 'smtp_enabled', value: smtp.enabled },
        { name: 'smtp_host', value: smtp.host },
        { name: 'smtp_user', value: smtp.user },
        { name: 'smtp_password', value: smtp.password },
        { name: 'smtp_auth', value: smtp.auth },
        { name: 'smtp_security', value: smtp.security },
        { name: 'smtp_from', value: smtp.from }
    ];
}
