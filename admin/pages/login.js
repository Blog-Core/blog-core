var fs = require('fs');
var api = require('../api');
var view = require('../view');
var validate = require('../validate');

var template = fs.readFileSync(__dirname + '/login.html', { encoding: 'utf8' });

// The login page.

exports.create = function() {

    var model = {

        username: ko.observable('').trimmed(),

        password: ko.observable('').trimmed(),

        remember: ko.observable(false)
    };

    model.login = function(form) {

        validate.clear(form);

        var username = model.username();

        var password = model.password();

        if (username === '') {

            validate.error('username', 'Username is not entered.');

        } else if (!username.match(/^[^@]+@[^@]+$/)) {

            validate.error('username', 'Username must be an email.');
        }

        if (password === '') {

            validate.error('password', 'Password is not entered.');
        }

        if (validate.hasError(form)) {

            return false;
        }

        api.login(username, password).then(function(res) {

            if (model.remember()) {

                localStorage.setItem('api-key', res.key);

                sessionStorage.removeItem('api-key');

            } else {

                sessionStorage.setItem('api-key', res.key);

                localStorage.removeItem('api-key');
            }

            route.go('landing');

        }).catch(function(err) {

            validate.formError(form, err);

        });
    };

    view.show(template, model);

    return Promise.resolve();
};
